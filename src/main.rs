use arrayvec::{ArrayVec, IntoIter};
use bevy::prelude::*;
use bevy_ascii_terminal as bat;
use bevy_ascii_terminal::{StringFormatter, TileFormatter};
use glam::IVec2;
use noise::utils::{NoiseMapBuilder, PlaneMapBuilder};
use noise::Fbm;
use rand::seq::SliceRandom;
use rand::{seq::IteratorRandom, thread_rng, Rng};
use sark_grids::prelude::*;
use sark_pathfinding::{pathmap::PathMap, Pathfinder};

const PATH_GLYPHS: [[char; 3]; 16] = [
    ['·', '·', '·'],
    ['·', '│', '║'],
    ['·', '─', '═'],
    ['·', '┘', '╝'],
    ['·', '─', '═'],
    ['·', '└', '╚'],
    ['·', '─', '═'],
    ['·', '┴', '╩'],
    ['·', '│', '║'],
    ['·', '│', '║'],
    ['·', '┐', '╗'],
    ['·', '┤', '╣'],
    ['·', '┌', '╔'],
    ['·', '├', '╠'],
    ['·', '┬', '╦'],
    ['·', '┼', '╬'],
];

const OPEN_TILE: bat::Tile = bat::Tile {
    glyph: ' ',
    fg_color: Color::BLACK,
    bg_color: Color::BLACK,
};
const TREE_TILE: bat::Tile = bat::Tile {
    glyph: 't',
    fg_color: Color::GREEN,
    bg_color: Color::BLACK,
};
const WATER_TILE: bat::Tile = bat::Tile {
    glyph: '█',
    fg_color: Color::BLUE,
    bg_color: Color::BLACK,
};
const DISTRICT_TILE: bat::Tile = bat::Tile {
    glyph: '@',
    fg_color: Color::FUCHSIA,
    bg_color: Color::BLACK,
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum MapTileKind {
    OPEN,
    TREE,
    PATH,
    WATER,
    DISTRICT,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct MapTile {
    kind: MapTileKind,
    glyph: char,
    glyph_idx: usize,
    value: u32,
}

impl Default for MapTile {
    fn default() -> Self {
        MapTile {
            kind: MapTileKind::OPEN,
            glyph: '·',
            glyph_idx: 0,
            value: 0,
        }
    }
}

#[derive(Debug)]
pub struct WorldMap {
    grid: Grid<MapTile>,
}

impl WorldMap {
    pub fn new(size: impl Size2d) -> Self {
        Self {
            grid: Grid::default(size),
        }
    }
}

#[derive(Resource)]
struct Settings {
    show_merchants: bool,
    show_help: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            show_merchants: true,
            show_help: true,
        }
    }
}

#[derive(Component)]
struct Merchant {
    timer: Timer,
    pos: [i32; 2],
    destination: [i32; 2],
}

#[derive(Resource)]
struct Map(WorldMap);

#[derive(Resource)]
struct Pf(Pathfinder);

#[derive(Resource)]
struct Districts(Vec<IVec2>);

impl std::ops::Deref for WorldMap {
    type Target = Grid<MapTile>;

    fn deref(&self) -> &Self::Target {
        &self.grid
    }
}

impl std::ops::DerefMut for WorldMap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.grid
    }
}

impl WorldMap {
    fn get_path_bitmask(&self, p: impl GridPoint) -> u32 {
        let mut bitmask: u32 = 0;
        // clean this up!
        if self.in_bounds(p.up(1))
            && (self[p.up(1)].kind == MapTileKind::PATH
                || self[p.up(1)].kind == MapTileKind::DISTRICT)
        {
            bitmask += 1;
        }
        if self.in_bounds(p.left(1))
            && (self[p.left(1)].kind == MapTileKind::PATH
                || self[p.left(1)].kind == MapTileKind::DISTRICT)
        {
            bitmask += 2;
        }
        if self.in_bounds(p.right(1))
            && (self[p.right(1)].kind == MapTileKind::PATH
                || self[p.right(1)].kind == MapTileKind::DISTRICT)
        {
            bitmask += 4;
        }
        if self.in_bounds(p.down(1))
            && (self[p.down(1)].kind == MapTileKind::PATH
                || self[p.down(1)].kind == MapTileKind::DISTRICT)
        {
            bitmask += 8;
        }
        bitmask
    }
}

impl PathMap for WorldMap {
    type ExitIterator = IntoIter<IVec2, 4>;
    fn exits(&self, p: impl GridPoint) -> Self::ExitIterator {
        let mut points = ArrayVec::new();
        for adj in p.adj_4() {
            if !self.in_bounds(adj) {
                continue;
            }
            points.push(adj);
        }
        points.into_iter()
    }

    fn cost(&self, _: impl GridPoint, b: impl GridPoint) -> i32 {
        match self.grid[b].kind {
            MapTileKind::DISTRICT => 1,
            MapTileKind::PATH => 2,
            MapTileKind::OPEN => 10,
            MapTileKind::TREE => 20,
            MapTileKind::WATER => 10000,
        }
    }

    fn distance(&self, a: impl GridPoint, b: impl GridPoint) -> usize {
        a.taxi_dist(b)
    }
}

struct Start;

fn main() {
    App::new()
        .add_event::<Start>()
        .insert_resource(ClearColor(Color::hex("727272").unwrap()))
        .add_plugins(DefaultPlugins)
        .add_plugin(bat::TerminalPlugin)
        .insert_resource(Settings::default())
        .add_startup_system(setup)
        .add_system(build_world)
        .add_system(draw)
        .add_system(merchant_travel)
        .add_system(keyboard_input)
        .run()
}

    
fn setup(mut commands: Commands, mut start_evt: EventWriter<Start>) {
    let size: [u32; 2] = [80, 40];
    let map = WorldMap::new(size);

    commands.spawn((bat::TerminalBundle::new().with_size(size), bat::AutoCamera));

    commands.insert_resource(Map(map));
    commands.insert_resource(Pf(Pathfinder::new()));
    commands.insert_resource(Districts(vec![]));

    start_evt.send(Start {});
}

fn build_world(mut commands: Commands, mut start_evt: EventReader<Start>, q_merchants: Query<Entity, With<Merchant>>) {
    for _ev in start_evt.iter() {
        for m in q_merchants.iter() {
            commands.entity(m).despawn();
        }

        let size: [u32; 2] = [80, 40];
        let mut map = WorldMap::new(size);

        let fbm = Fbm::<noise::Perlin>::new(thread_rng().gen());
        let plane = PlaneMapBuilder::<_, 2>::new(&fbm)
            .set_size(map.width(), map.height())
            .set_x_bounds(-2.0, 2.0)
            .set_y_bounds(-2.0, 2.0)
            .build();

        let w = map.width();
        for (i, m) in map.iter_mut().enumerate() {
            let x = i % w;
            let y = i / w;
            let v = plane.get_value(x, y);

            if v >= 0.4 {
                *m = MapTile {
                    kind: MapTileKind::TREE,
                    value: 10,
                    ..default()
                };
            } else if v <= -0.85 {
                *m = MapTile {
                    kind: MapTileKind::WATER,
                    ..default()
                };
            }
        }

        let mut districts: Vec<IVec2> = vec![];
        for _ in 0..10 {
            loop {
                let x = thread_rng().gen_range(0..map.width());
                let y = thread_rng().gen_range(0..map.height());
                if map[[x, y]].kind != MapTileKind::WATER {
                    map[[x, y]] = MapTile {
                        kind: MapTileKind::DISTRICT,
                        ..default()
                    };
                    districts.push(IVec2::new(x as i32, y as i32));
                    break;
                }
            }
        }

        for _ in 0..15 {
            let d = districts.choose(&mut thread_rng()).unwrap();
            commands.spawn(Merchant {
                pos: Into::<[i32; 2]>::into(*d),
                timer: Timer::from_seconds(0.1, TimerMode::Repeating),
                destination: [-1, -1],
            });
        }

        commands.insert_resource(Map(map));
        commands.insert_resource(Pf(Pathfinder::new()));
        commands.insert_resource(Districts(districts));
    }
}

fn draw(
    mut q_term: Query<&mut bat::Terminal>,
    map: Res<Map>,
    q_merchants: Query<&Merchant>,
    settings: ResMut<Settings>,
) {
    let mut term = q_term.single_mut();

    for (map_tile, tile) in map.0.iter().zip(term.iter_mut()) {
        match map_tile.kind {
            MapTileKind::OPEN => *tile = OPEN_TILE,
            MapTileKind::TREE => *tile = TREE_TILE,
            MapTileKind::WATER => *tile = WATER_TILE,
            MapTileKind::DISTRICT => *tile = DISTRICT_TILE,
            MapTileKind::PATH => {
                *tile = bat::Tile {
                    glyph: map_tile.glyph,
                    fg_color: Color::GRAY,
                    bg_color: Color::BLACK,
                };
            }
        }
    }

    if settings.show_merchants {
        for m in q_merchants.iter() {
            term.put_char(m.pos, '•'.fg(Color::WHITE));
        }
    }

    if settings.show_help {
        let top: i32 = map.0.height().try_into().unwrap();
        term.put_string(
            IVec2::new(0, top - 1),
            "(m) toggle merchants".fg(Color::WHITE).bg(Color::DARK_GRAY),
        );
        term.put_string(
            IVec2::new(0, top - 2),
            "(h) toggle help     ".fg(Color::WHITE).bg(Color::DARK_GRAY),
        );
        term.put_string(
            IVec2::new(0, top - 3),
            "(r) restart         ".fg(Color::WHITE).bg(Color::DARK_GRAY),
        );
    }
}

fn merchant_travel(
    mut q_merchants: Query<&mut Merchant>,
    time: Res<Time>,
    mut map: ResMut<Map>,
    mut pf: ResMut<Pf>,
    districts: Res<Districts>,
) {
    for mut m in q_merchants.iter_mut() {
        m.timer.tick(time.delta());
        if m.timer.finished() {
            let mpos: IVec2 = m.pos.into();
            if districts.0.contains(&mpos) {
                m.destination = Into::<[i32; 2]>::into(
                    *districts
                        .0
                        .iter()
                        .filter(|&d| d != &mpos)
                        .choose(&mut thread_rng())
                        .unwrap(),
                );
            }

            if let Some(path) = pf.0.astar(&map.0, m.pos, m.destination) {
                let idx: [i32; 2] = path[1].into();
                if map.0[idx].kind == MapTileKind::TREE {
                    if map.0[idx].value > 0 {
                        map.0[idx].value -= 1;
                    } else {
                        map.0[idx].kind = MapTileKind::OPEN;
                    }
                } else {
                    m.pos = path[1].into();

                    if thread_rng().gen_range(0.0..1.0) < 0.5 {
                        let mut bitmask: u32;
                        if map.0[m.pos].kind == MapTileKind::OPEN {
                            map.0[m.pos].kind = MapTileKind::PATH;
                            bitmask = map.0.get_path_bitmask(m.pos);
                            map.0[m.pos].glyph =
                                PATH_GLYPHS[bitmask as usize][map.0[m.pos].glyph_idx];

                            // clunky!
                            for adj in m.pos.adj_4() {
                                if !map.0.in_bounds(adj) {
                                    continue;
                                };
                                bitmask = map.0.get_path_bitmask(adj);
                                map.0[adj].glyph =
                                    PATH_GLYPHS[bitmask as usize][map.0[m.pos].glyph_idx];
                            }
                        } else if map.0[m.pos].kind == MapTileKind::PATH {
                            map.0[m.pos].value += 1;
                            if map.0[m.pos].glyph_idx == 2 {
                                // do nothing, path already at highest level
                            } else if map.0[m.pos].value >= 50 {
                                map.0[m.pos].glyph_idx = 2;
                                bitmask = map.0.get_path_bitmask(m.pos);
                                map.0[m.pos].glyph =
                                    PATH_GLYPHS[bitmask as usize][map.0[m.pos].glyph_idx];
                            } else if map.0[m.pos].value >= 10 {
                                map.0[m.pos].glyph_idx = 1;
                                bitmask = map.0.get_path_bitmask(m.pos);
                                map.0[m.pos].glyph =
                                    PATH_GLYPHS[bitmask as usize][map.0[m.pos].glyph_idx];
                            }
                        }
                    }
                }
            }
        }
    }
}

fn keyboard_input(keys: Res<Input<KeyCode>>, mut settings: ResMut<Settings>, mut start_evt: EventWriter<Start>) {
    if keys.just_pressed(KeyCode::M) {
        settings.show_merchants = !settings.show_merchants;
    }

    if keys.just_pressed(KeyCode::H) {
        settings.show_help = !settings.show_help;
    }

    if keys.just_pressed(KeyCode::R) {
        start_evt.send(Start {})
    }
}
