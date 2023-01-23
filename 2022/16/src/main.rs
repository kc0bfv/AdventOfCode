use std::collections::{HashMap, hash_map::Keys, HashSet};

type Rate = u32;
type Time = u32;
type NodeName = String;

struct Link {
    dest: String,
    dist: Time,
}

struct Node {
    _name: String,
    rate: Rate,
    links: HashMap<String, Link>,
}

struct Graph {
    nodes: HashMap<NodeName, Node>,
}

impl Graph {
    fn new() -> Self {
        Self { nodes: HashMap::new() }
    }

    fn add_line(self: &mut Self, mut line: std::str::Split<&str>) -> () {
        assert!(line.next().unwrap() == "Valve");

        let name = line.next().expect("Failed on name");

        assert!(line.next().unwrap() == "has");
        assert!(line.next().unwrap() == "flow");

        let rate_str = line.next().expect("Failed on rate");
        let rate = rate_str[5..(rate_str.len()-1)].parse().expect("Failed to parse rate");

        line.next().unwrap(); // tunnel or tunnels
        line.next().unwrap(); // leads or lead
        assert!(line.next().unwrap() == "to");
        line.next().unwrap(); // valve or valves

        let links = line
                .map(|raw_nm| raw_nm[0..2].into())
                .map(|nm| (nm, 1))
                .collect();

        self.add_node(name, rate, links);
    }

    fn add_node(&mut self, n_nm: &str, rate: Rate, links_in: Vec<(String, Time)>) {
        let links = HashMap::from_iter(
            links_in
                .iter()
                .map(|(nm, dist)| (nm.into(), Link { dest: nm.into(), dist: *dist }))
        );
        let node = Node { _name: n_nm.into(), rate: rate, links: links };
        self.nodes.insert(n_nm.into(), node);
    }

    fn get_node(&self, n_nm: &str) -> &Node {
        self.nodes.get(n_nm).expect("Failed to get node")
    }

    fn node_names(&self) -> Keys<String, Node> {
        self.nodes.keys()
    }

    fn shortest_dists(&self, st_n_nm: &str) -> Vec<(String, Time)> {
        let mut visited: HashMap<String, Option<Time>> = HashMap::from_iter(
            self.nodes
                .keys()
                .map(|nm| (nm.into(), None))
        );

        let mut cur_dist: Time = 0;

        let mut frontier: HashSet<String> = HashSet::new();
        frontier.insert(st_n_nm.into());

        // While something hasn't been visited
        while visited.values().any(|val| val.is_none()) {
            // If the frontier is empty, we're broken, so die
            assert!(frontier.len() > 0);

            let mut new_frontier: HashSet<String> = HashSet::new();

            // Visit every node in the frontier
            for n_nm in frontier {
                // If this is our first time there, set the dist to it
                // dist only ever gets bigger, so we only need to initialize...
                if visited.get(&n_nm).expect("Failed to find node in vis").is_none() {
                    visited.insert((&n_nm).into(), Some(cur_dist));
                }

                // Take all of its children and add them to new_frontier if they weren't visited yet
                for (lnk_dest_nm, _) in self.get_node(&n_nm).links.iter() {
                    if visited.get(lnk_dest_nm.into()).expect("Failed fo find lnk in vis").is_none() {
                        new_frontier.insert(lnk_dest_nm.into());
                    }
                }

            }

            cur_dist += 1;
            frontier = new_frontier;
        }

        // Prep retval
        visited
            .iter()
            .map(|(name, opt_time)| (name.into(), opt_time.expect("Found none in ret")))
            .collect()
    }
}

impl std::fmt::Display for Graph {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.nodes.iter().map(|(n_nm, node)| {
            write!(f, "Node {}, rate {}: ", n_nm, node.rate)?;
            
            node.links.iter().map(|(_, link)|
                write!(f, "{}-{} ", link.dest, link.dist)
            ).collect::<std::fmt::Result>()?;

            write!(f, "\n")
        }).collect()
    }
}

fn reduce_graph(graph: &Graph) -> Graph {
    let all_paths = graph.node_names().map(|n_nm| (n_nm, graph.shortest_dists(n_nm)));
    let mut new_graph = Graph::new();
    all_paths.for_each(|(n_nm, dists)| {
        let reduced_dists = dists
            .iter()
            .filter(|(n_nm, _)| graph.get_node(n_nm).rate > 0 )
            .map(|(n_nm, dist)| (n_nm.clone(), dist.clone()))
            .collect();
        new_graph.add_node(n_nm, graph.get_node(n_nm).rate, reduced_dists)
    });

    new_graph
}

fn dfs_maximize(graph: &Graph, st_n_nm: &str, time_left: Time, visited: &HashSet<String>)
    -> Rate
{
    if time_left <= 0 {
        return 0;
    }

    let st_n_contrib = graph.get_node(st_n_nm).rate * time_left;

    let mut new_visited = visited.clone();
    new_visited.insert(st_n_nm.into());

    graph
        .get_node(st_n_nm)
        .links
        .iter()
        .map(|(dest_n_nm, link)| {
            if new_visited.contains(dest_n_nm) || time_left < 1 + link.dist {
                return 0;
            }

            dfs_maximize(graph, dest_n_nm, time_left - 1 - link.dist, &new_visited)
        })
        .max().unwrap_or(0) + st_n_contrib
}

fn runit(filename: &str) -> Result<(), Box<dyn std::error::Error>> {
    let mut initial_graph = Graph::new();

    std::fs::read_to_string(filename)?
        .lines()
        .map(|line| line.trim().split(" "))
        .for_each(|line| initial_graph.add_line(line));

    let reduced_graph = reduce_graph(&initial_graph);

    println!("Graph: {}", initial_graph);
    println!("Reduced graph: {}", reduced_graph);

    let p1 = dfs_maximize(&reduced_graph, "AA", 30, &HashSet::new());

    println!("Part 1: {}", p1);

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    runit("samp")?;
    runit("input.txt")?;

    Ok(())
}
