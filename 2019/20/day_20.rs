use std::env::args;
use std::collections::{HashMap, HashSet};

pub mod file_help;
pub mod position;

use file_help::read_strs;
use position::Position;

type PortalName = (char, char);

type NodeData = PortalName;
type NodeDataP2 = (NodeData, EdgeInOut, usize);
type EdgeWeight = u32;

#[derive(Debug,Clone,PartialEq,Eq,Hash)]
enum EdgeInOut { Inner, Outer }

#[derive(Debug,Clone)]
struct Edge {
    node_1: usize,
    node_1_io: EdgeInOut,
    node_2: usize,
    node_2_io: EdgeInOut,
    weight: EdgeWeight,
}

#[derive(Debug)]
struct Graph {
    nodes: Vec<NodeData>,
    edges: Vec<Edge>,
}
impl Graph {
    fn new(posses: Vec<NodeData>) -> Self {
        Self{ nodes: posses, edges: vec![] }
    }
    fn find_node(&self, node: &NodeData) -> usize {
        let n_inds: Vec<usize> = self.nodes.iter()
                .cloned()
                .enumerate()
                .filter(|(_, dat)| *dat == *node)
                .map(|(ind, _)| ind)
                .collect();
        assert!(n_inds.len() == 1);
        return n_inds[0];
    }
    fn add_edge(&mut self, node_1: &NodeData, node_1_io: &EdgeInOut,
            node_2: &NodeData, node_2_io: &EdgeInOut,
            weight: EdgeWeight) {
        let n_1_ind = self.find_node(node_1);
        let n_2_ind = self.find_node(node_2);
        self.edges.push(Edge { node_1: n_1_ind, node_1_io: node_1_io.clone(),
                node_2: n_2_ind, node_2_io: node_2_io.clone(), weight });
    }
    fn find_edge(&mut self, node_1: &NodeData, node_2: &NodeData)
            -> Option<EdgeWeight> {
        let n_1_ind = self.find_node(node_1);
        let n_2_ind = self.find_node(node_2);
        let matches: Vec<&Edge> = self.edges
                .iter()
                .filter(|Edge{ node_1: en1, node_2: en2, .. }|
                        (*en1 == n_1_ind && *en2 == n_2_ind)
                        || (*en1 == n_2_ind && *en2 == n_1_ind))
                .collect();
        if matches.len() > 1 {
            println!("More than one edge match {:?} {:?}", node_1, node_2);
        }
        match matches.get(0) {
            Some(Edge { weight: w, .. }) => Some(w.clone()),
            None => None,
        }
    }
    fn shortest_path(&self, node_1: &NodeData, node_2: &NodeData)
            -> Vec<(NodeData, EdgeWeight)>
    {
        let mut weights: HashMap<NodeData, EdgeWeight> = HashMap::new();
        let mut open: HashSet<NodeData> = HashSet::new();
        let mut visited: HashSet<NodeData> = HashSet::new();

        self.nodes.iter().cloned()
                .for_each(|nd| { weights.insert(nd, std::u32::MAX); });

        open.insert(node_1.clone());
        weights.insert(node_1.clone(), 0);

        // Build up the weights
        while open.len() > 0 {
            let cur: NodeData = open.iter()
                .min_by_key(|nd| weights.get(nd).unwrap())
                .unwrap()
                .clone();
            let cur_ind: usize = self.find_node(&cur);
            open.remove(&cur);
            visited.insert(cur);
            if cur == *node_2 {
                break;
            }
            let weight = weights.get(&cur).unwrap().clone();
            let edges: Vec<Edge> = self.edges.iter()
                .filter(|Edge{node_1: en1, node_2: en2, ..}| *en1 == cur_ind || *en2 == cur_ind)
                .cloned()
                .collect();
            for Edge{node_1: en1, node_2: en2, weight: ed_wt, ..} in edges {
                let other_n: NodeData =
                        (if en1 == cur_ind { self.nodes[en2] } else { self.nodes[en1] }).clone();
                if visited.contains(&other_n) {
                    continue;
                }
                if *(weights.get(&other_n).unwrap()) > weight+ed_wt {
                    weights.insert(other_n, weight+ed_wt);
                }
                open.insert(other_n);
            }
        }

        // Walk the path
        let mut path: Vec<(NodeData, EdgeWeight)> = vec![];
        let mut cur: NodeData = node_2.clone();
        while cur != *node_1 {
            let cur_ind: usize = self.find_node(&cur);
            path.push((cur, *(weights.get(&cur).unwrap())));
            let connected: Vec<NodeData> = self.edges.iter()
                .filter(|Edge{node_1: en1, node_2: en2, ..}| *en1 == cur_ind || *en2 == cur_ind)
                .map(|Edge{node_1: en1, node_2: en2, ..}|
                        (if *en1 == cur_ind { self.nodes[*en2] } else { self.nodes[*en1] }).clone())
                .collect();
            let best_e: NodeData = connected.into_iter()
                .min_by_key(|nd| weights.get(&nd).unwrap())
                .unwrap();
            cur = best_e;
        }

        return path;
    }

    fn shortest_path_part_2(&self, node_1: &NodeDataP2, node_2: &NodeDataP2)
            -> EdgeWeight
    {
        let mut weights: HashMap<NodeDataP2, EdgeWeight> = HashMap::new();
        let mut open: HashSet<NodeDataP2> = HashSet::new();
        let mut visited: HashSet<NodeDataP2> = HashSet::new();

        /*
        self.nodes.iter().cloned()
                .for_each(|nd| { weights.insert(nd, std::u32::MAX); });
        */

        open.insert(node_1.clone());
        weights.insert(node_1.clone(), 0);

        let st_ind = self.find_node(&('A', 'A'));
        let ed_ind = self.find_node(&('Z', 'Z'));

        // Build up the weights
        while open.len() > 0 {
            let cur: NodeDataP2 = open.iter()
                .min_by_key(|nd| weights.get(nd).unwrap())
                .unwrap()
                .clone();
            open.remove(&cur);
            visited.insert(cur.clone());
            if cur == *node_2 {
                break;
            }
            let weight = weights.get(&cur).unwrap().clone();
            if cur.2 > 1000 {
                panic!("Avoiding traversing past depth 1000...  At {:?} weight {}", cur, weight);
            }
            let cur_ind: usize = self.find_node(&cur.0);
            let edges: Vec<Edge> = self.edges.iter()
                // Only take edges that include the cur node
                .filter(|Edge{node_1: en1, node_2: en2, ..}| 
                        *en1 == cur_ind || *en2 == cur_ind)
                // Ignore edges containing st and ed if not on depth 0 or 1
                .filter(|Edge{node_1: en1, node_2: en2, ..}| 
                        ! (cur.2 > 1 && ( *en1 == st_ind || *en2 == st_ind
                                || *en1 == ed_ind || *en2 == ed_ind)
                            )
                        )
                /*
                // Ignore outer edges other than st/ed at depth 0
                .filter(|edge| {
                        let other_edge_io = 
                                if edge.node_1 == cur_ind { edge.node_2_io.clone() }
                                else { edge.node_1_io.clone() };
                        ! (cur.2 == 0 && other_edge_io == EdgeInOut::Outer)
                    })
                */
                .cloned()
                .collect();
            for Edge{node_1: en1, node_1_io: en1io, node_2: en2,
                    node_2_io: en2io, weight: ed_wt} in edges
            {
                let (_cur_match, other_match) = 
                        if en1 == cur_ind { (en1, en2) }
                        else { (en2, en1) };
                let (cur_io_match, other_io_match) =
                        if en1 == cur_ind { (en1io, en2io) }
                        else { (en2io, en1io) };
                let other_depth: usize = 
                        // Now, if cur's matching io has the same inner/outer
                        // as the current search node, we haven't changed depth
                        // Otherwise, we have gone deeper/higher
                        if cur_io_match == cur.1 {
                            cur.2
                        } else if cur.1 == EdgeInOut::Inner {
                            cur.2 + 1
                        } else {
                            if cur.2 == 0 {
                                panic!("Trying to add {:?} {:?} at {:?}", self.nodes[other_match], other_io_match, cur);
                            }
                            cur.2 - 1
                        };
                let other_n: NodeDataP2 =
                        (self.nodes[other_match], other_io_match, other_depth).clone();

                if visited.contains(&other_n) {
                    continue;
                }
                let new_weight = weight + ed_wt;
                // If there's no old weight, or a better new_weight, store it
                match weights.get(&other_n) {
                    Some(old_weight) if (*old_weight <= new_weight) => (),
                    _ => { weights.insert(other_n.clone(), weight+ed_wt); },
                }
                if other_n.0 == ('A','A') {
                    //println!("Skipping adding st ever - {:?} at {:?}", other_n, cur);
                } else if other_n.2 != 0 && other_n.0 == ('Z','Z') {
                    //println!("Skipping adding ed at non 0 - {:?} at {:?}", other_n, cur);
                } else if other_n.1 == EdgeInOut::Outer && other_n.2 == 0 && other_n.0 != ('Z','Z') {
                    //println!("Skipping adding {:?} at {:?}", other_n, cur);
                } else {
                    open.insert(other_n);
                }
            }
        }

        // Walk the path
        /*
        let mut path: Vec<(NodeDataP2, EdgeWeight)> = vec![];
        let mut cur: NodeDataP2 = node_2.clone();
        while cur != *node_1 {
            let cur_ind: usize = self.find_node(&cur.0);
            path.push((cur, *(weights.get(&cur).unwrap())));
            let connected: Vec<NodeDataP2> = self.edges.iter()
                .filter(|Edge{node_1: en1, node_2: en2, ..}| *en1 == cur_ind || *en2 == cur_ind)
                .map(|Edge{node_1: en1, node_2: en2, ..}|
                        (if *en1 == cur_ind { self.nodes[*en2] } else { self.nodes[*en1] }).clone())
                .collect();
            let best_e: NodeDataP2 = connected.into_iter()
                .min_by_key(|nd| weights.get(&nd).unwrap())
                .unwrap();
            cur = best_e;
        }

        return path;
        */
        return weights.get(&node_2).unwrap().clone()
    }
}

#[derive(Debug)]
enum SpotType {
    Start,
    End,
    Wall,
    Hall,
    Portal(PortalName),
}

#[derive(Debug)]
struct Pluto {
    x_min: usize, y_min: usize, x_max: usize, y_max: usize,
    inner_x_min: usize, inner_y_min: usize,
    inner_x_max: usize, inner_y_max: usize,
    start: Position,
    end: Position,
    portals: HashMap<PortalName, (Position, Position)>,
    graph: Graph,
    map: HashMap<Position, SpotType>,
}
impl Pluto {
    fn new(lines: &Vec<Vec<String>>) -> Self {
        let ((x_min, y_min, x_max, y_max),
            (inner_x_min, inner_y_min, inner_x_max, inner_y_max),
            (start, end),
            map, portal_names) = 
            Pluto::build_hash_map(lines);
           
        /*
        use dijkstra to find shortest path through graph
        */
        let mut nodes: HashSet<PortalName> = HashSet::new();
        let mut node_pos_1: HashMap<PortalName, Position> = HashMap::new();
        let mut portals: HashMap<PortalName, (Position, Position)> =
                HashMap::new();
        for (pos, name) in portal_names {
            nodes.insert(name);
            if node_pos_1.contains_key(&name) { 
                portals.insert(name.clone(),
                        (node_pos_1.get(&name).unwrap().clone(), pos)
                    );
            } else {
                node_pos_1.insert(name.clone(), pos);
            }
        }
        let mut graph = Graph::new(nodes.iter().cloned()
                    .chain(vec![('A', 'A')].into_iter())
                    .chain(vec![('Z', 'Z')].into_iter())
                    .collect()
                );
        Self::build_graph_edges(&map, &mut graph, &portals, &start, &end,
            y_min, y_max, x_min, x_max);

        Pluto {
            x_min, y_min, x_max, y_max,
            inner_x_min, inner_y_min, inner_x_max, inner_y_max,
            start, end, portals, graph, map
        }
    }

    fn build_graph_edges(map: &HashMap<Position, SpotType>, graph: &mut Graph, 
            portals: &HashMap<PortalName, (Position, Position)>,
            start: &Position, end: &Position,
            y_min: usize, y_max: usize, x_min: usize, x_max: usize,
            )
    {
        let mut st_poses: Vec<Position> = vec![start.clone(), end.clone()];
        for (pos_1, pos_2) in portals.values() {
            st_poses.push(pos_1.clone());
            st_poses.push(pos_2.clone());
        }

        for st_pos in st_poses {
            let st_name: PortalName = match map.get(&st_pos) {
                    Some(SpotType::Start) => ('A', 'A'),
                    Some(SpotType::End) => ('Z', 'Z'),
                    Some(SpotType::Portal(name)) => name.clone(),
                    None => panic!("Found invalid start pos {:?}", st_pos),
                    _ => panic!("Unexpected spot type for start {:?}", st_pos),
                };
            let st_io: EdgeInOut =
                if st_pos.y == y_min || st_pos.y == y_max
                    || st_pos.x == x_min || st_pos.x == x_max
                {
                    EdgeInOut::Outer
                } else { EdgeInOut::Inner };
            //println!("Pos {:?}, name {:?}", st_pos, st_name);

            // Breadth-first-search to every portal
            let mut open_pos: HashSet<Position> = HashSet::new();
            let mut weights: HashMap<Position, EdgeWeight> = HashMap::new();
            open_pos.insert(st_pos.clone());
            weights.insert(st_pos.clone(), 0);

            let mut visited: HashSet<Position> = HashSet::new();

            while open_pos.len() > 0 {
                let pos = open_pos.iter()
                    .min_by_key(|pos| weights.get(pos).unwrap())
                    .unwrap()
                    .clone();
                open_pos.remove(&pos);

                let weight = weights.get(&pos).expect("Getting position weight");
                let new_weight = weight + 1;
                if visited.contains(&pos) { continue; }
                visited.insert(pos.clone());

                // See if the spot's in the map and get its type
                let spot = match map.get(&pos) {
                    Some(spt_typ) => spt_typ,
                    None => continue,
                };

                // If the spot is a hall, add surroundings to open
                let was_hall: bool = match spot { SpotType::Hall => true,
                        _ => false };
                //println!("{:?} {:?} {}", pos, st_pos, was_hall);
                if was_hall || (pos == st_pos) {
                    let nexts = pos.get_surroundings();
                    //println!("nexts {:?}", nexts);
                    nexts.iter()
                        .filter(|pos| match map.get(pos) {
                                None | Some(SpotType::Wall) => false,
                                _ => true,
                            })
                        .filter(|pos| !visited.contains(pos))
                        .for_each(|pos| {
                                //println!("pos {:?}", pos);
                                if !weights.contains_key(pos) || 
                                    *(weights.get(pos).unwrap()) > new_weight
                                {
                                    weights.insert(pos.clone(),
                                        new_weight.clone());
                                }
                                open_pos.insert(pos.clone());
                            });
                    continue;
                };
                // Otherwise add an edge
                let end_name: PortalName = match spot {
                        SpotType::Start => ('A', 'A'),
                        SpotType::End => ('Z', 'Z'),
                        SpotType::Portal(name) => name.clone(),
                        _ => panic!("Unexpected spot in portalname"),
                    };
                let end_io: EdgeInOut =
                    if pos.y == y_min || pos.y == y_max
                        || pos.x == x_min || pos.x == x_max
                    {
                        EdgeInOut::Outer
                    } else { EdgeInOut::Inner };
                match graph.find_edge(&st_name, &end_name) {
                    Some(weight) => if weight > new_weight {
                            panic!("Found confusing smaller edge {:?} {:?} {} {}",
                                    st_name, end_name, weight, new_weight);
                        },
                    None => { graph.add_edge(&st_name, &st_io, &end_name,
                            &end_io, new_weight); },
                };
            }
        }
    }

    fn build_hash_map(lines: &Vec<Vec<String>>)
        -> ((usize, usize, usize, usize), (usize, usize, usize, usize), 
            (Position, Position),
            HashMap<Position, SpotType>,
            Vec<(Position, PortalName)>,
            )
    {
        let y_min: usize = 2;
        let x_min: usize = 2;
        let y_max: usize = lines.len() - 3;
        let x_max: usize = lines[0][0].len() - 3;

        // Find the inner extents
        let y_mid_cont: Vec<(usize, char)> = (y_min..=y_max)
            .map(|y| (y, lines[y][0].chars().nth((x_max-x_min)/2).unwrap()))
            .filter(|(_, val)| *val != '#' && *val != '.')
            .collect();
        let inner_y_min: usize = y_mid_cont.iter()
                .min_by_key(|(y, _)| y).unwrap().0 - 1;
        let inner_y_max: usize = y_mid_cont.iter()
                .max_by_key(|(y, _)| y).unwrap().0 + 1;
        let x_mid_cont: Vec<(usize, char)> = (x_min..=x_max)
            .map(|x| (x, lines[(y_max-y_min)/2][0].chars().nth(x).unwrap()))
            .filter(|(_, val)| *val != '#' && *val != '.')
            .collect();
        let inner_x_min: usize = x_mid_cont.iter()
                .min_by_key(|(x, _)| x).unwrap().0 - 1;
        let inner_x_max: usize = x_mid_cont.iter()
                .max_by_key(|(x, _)| x).unwrap().0 + 1;

        // Build the hash map and portal position list
        let mut hm: HashMap<Position, SpotType> = HashMap::new();
        let mut portal: Vec<Position> = vec![];
        for y in y_min..=y_max {
            let line_str = &lines[y][0];
            for x in x_min..=x_max {
                let pos = Position::new(x, y);
                let char = line_str.chars().nth(x).unwrap();
                match char {
                    '.' => {
                            if y == y_min || y == y_max ||
                                x == x_min || x == x_max
                                || ((y == inner_y_min || y == inner_y_max)
                                    && x > inner_x_min && x < inner_x_max)
                                || ((x == inner_x_min || x == inner_x_max)
                                    && y > inner_y_min && y < inner_y_max)
                            {
                                portal.push(pos.clone());
                                // Build the portals properly later
                            }
                            hm.insert(pos, SpotType::Hall);
                        },
                    '#' => {
                            hm.insert(pos, SpotType::Wall);
                        },
                    _ => (),
                }
            }
        }

        // Resolve portal names, start and end
        let mut portal_names: Vec<(Position, PortalName)> = vec![];
        let mut start: Option<Position> = None;
        let mut end: Option<Position> = None;
        for port_pos in portal {
            let (char_1_pos, char_2_pos): (Position, Position) = {
                if port_pos.y == y_min {
                    (Position::new(port_pos.x, port_pos.y - 2),
                        Position::new(port_pos.x, port_pos.y - 1))
                } else if port_pos.y == y_max {
                    (Position::new(port_pos.x, port_pos.y + 1),
                        Position::new(port_pos.x, port_pos.y + 2))
                } else if port_pos.x == x_min {
                    (Position::new(port_pos.x - 2, port_pos.y),
                        Position::new(port_pos.x - 1, port_pos.y))
                } else if port_pos.x == x_max {
                    (Position::new(port_pos.x + 1, port_pos.y),
                        Position::new(port_pos.x + 2, port_pos.y))
                } else if port_pos.y == inner_y_min {
                    (Position::new(port_pos.x, port_pos.y + 1),
                        Position::new(port_pos.x, port_pos.y + 2))
                } else if port_pos.y == inner_y_max {
                    (Position::new(port_pos.x, port_pos.y - 2),
                        Position::new(port_pos.x, port_pos.y - 1))
                } else if port_pos.x == inner_x_min {
                    (Position::new(port_pos.x + 1, port_pos.y),
                        Position::new(port_pos.x + 2, port_pos.y))
                } else if port_pos.x == inner_x_max {
                    (Position::new(port_pos.x - 2, port_pos.y),
                        Position::new(port_pos.x - 1, port_pos.y))
                } else {
                    panic!("Unexpected position for a portal {:?}", port_pos);
                }
            };
            let char_1 = lines[char_1_pos.y][0].chars().nth(char_1_pos.x).unwrap();
            let char_2 = lines[char_2_pos.y][0].chars().nth(char_2_pos.x).unwrap();
            match (char_1, char_2) {
                ('A', 'A') => { start = Some(port_pos.clone()); },
                ('Z', 'Z') => { end = Some(port_pos.clone()); },
                (a, b)     => { portal_names.push((port_pos.clone(), (a, b))) },
            }
        }

        assert!(start.is_some() && end.is_some());

        // Set the names in the map
        hm.insert(start.clone().unwrap(), SpotType::Start);
        hm.insert(end.clone().unwrap(), SpotType::End);
        for (pos, name) in portal_names.iter().cloned() {
            hm.insert(pos, SpotType::Portal(name));
        }

        ((x_min, y_min, x_max, y_max),
            (inner_x_min, inner_y_min, inner_x_max, inner_y_max),
            (start.unwrap(), end.unwrap()),
            hm, portal_names)
    }

    fn print_map(&self) {
        for y in self.y_min..=self.y_max {
            for x in self.x_min..=self.x_max {
                let pos = Position::new(x, y);
                print!("{}",
                    match self.map.get(&pos) {
                        Some(SpotType::Start) => 'S',
                        Some(SpotType::End) => 'E',
                        Some(SpotType::Wall) => '#',
                        Some(SpotType::Hall) => '.',
                        Some(SpotType::Portal(_)) => 'P',
                        None => ' ',
                    }
                    );
            }
            println!("");
        }
    }
}

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_lines = read_strs(&filename, '%');

    let pluto = Pluto::new(&input_lines);
    pluto.print_map();
    //print!("Graph {:?}", pluto.graph);
    let shortest_path = pluto.graph.shortest_path(&('A', 'A'), &('Z', 'Z'));
    println!("{:?}", shortest_path);
    println!("Part 1: {}", shortest_path[0].1 - 1);
    let shortest_path = pluto.graph.shortest_path_part_2(
            &(('A', 'A'), EdgeInOut::Outer, 0),
            &(('Z', 'Z'), EdgeInOut::Outer, 0));
    println!("Part 2: {}", shortest_path - 1);
}
