use std::env::args;
use std::fmt;
use std::collections::HashMap;

pub mod file_help;
use file_help::read_strs;



#[derive(Debug)]
struct GraphNode {
    name: String,
    kids: Vec<Self>,
}

impl GraphNode {
    fn new(name: &String, orbit_data: &Vec<Vec<String>>) -> Self {
        let kid_strs = orbit_data.iter()
                .filter(|line_vec| line_vec[0] == *name)
                .map(|line_vec| &line_vec[1]);
        let kids = kid_strs.map(
                |k_n| GraphNode::new(&k_n, &orbit_data)).collect();
        Self{ name: name.clone(), kids: kids }
    }
    fn count_orbits(&self, depth: u32) -> u32 {
        depth + self.kids.iter().map(|kid| kid.count_orbits(depth+1)).sum::<u32>()
    }
    fn enum_nodes(self: &Self) -> Vec<(String, &GraphNode, &GraphNode)> {
        let mut ret_vec = Vec::<(String, &GraphNode, &GraphNode)>::new();
        for kid in self.kids.iter() {
            ret_vec.push((kid.name.clone(), &kid, &self));
            for enumed in kid.enum_nodes() {
                ret_vec.push(enumed);
            }
        }
        ret_vec
    }
}


#[derive(Debug)]
struct Graph {
    root: GraphNode,
}

impl Graph {
    fn new(orbit_data: &Vec<Vec<String>>) -> Graph {
        Graph { root: GraphNode::new(&String::from("COM"), orbit_data) }
    }
    fn fmt_graph_line(node: &GraphNode, f: &mut fmt::Formatter<'_>,
            spc_cnt: u32) -> fmt::Result
    {
        let spaces: String = (0..spc_cnt).map(|_| ' ').collect();
        match write!(f, "\n{}{}", spaces, node.name) {
            Ok(_) => (),
            Err(err) => panic!("Error writing name: {}", err),
        };
        node.kids.iter().fold(Ok(()), |accum, kid| {
                match accum {
                    Ok(_) => Graph::fmt_graph_line(&kid, f, spc_cnt+2),
                    Err(err) => panic!("Error formatting: {}", err),
                }
            })
    }

    fn count_orbits(&self) -> u32 {
        self.root.count_orbits(0)
    }

    fn find_dist_btwn(&self, name_1: &String, name_2: &String) -> u32 {
        let gr_map = self.build_graph_hash();
        let cmn_parent = self.find_min_common(name_1, name_2, &gr_map);
        let parents_1 = self.get_all_parents(name_1, &gr_map);
        let parents_2 = self.get_all_parents(name_2, &gr_map);

        let bef_1 = parents_1.iter().take_while(|item| item.name != cmn_parent.name);
        let bef_2 = parents_2.iter().take_while(|item| item.name != cmn_parent.name);

        return (bef_1.count() + bef_2.count()) as u32;
    }

    fn find_min_common<'a>(&self, name_1: &'a String, name_2: &'a String,
            gr_map: &HashMap<String, (&'a GraphNode, &'a GraphNode)>) -> &'a GraphNode
    {
        let parents_1 = self.get_all_parents(name_1, gr_map);
        let parents_2 = self.get_all_parents(name_2, gr_map);
        let parents_2_names: Vec<String> = parents_2.iter().map(|item| item.name.clone()).collect();
        for node in parents_1.iter() {
            if parents_2_names.contains(&node.name) {
                return node;
            }
        }
        panic!("Shouldn't get here");
    }
    
    fn get_all_parents<'a>(&self, name: &'a String,
            gr_map: &HashMap<String, (&'a GraphNode, &'a GraphNode)>) -> Vec<&'a GraphNode>
    {
        if name == "COM" {
            return Vec::new();
        }
        let parent = self.find_node_parent(name, gr_map);
        let mut ch_vec: Vec<&GraphNode> = vec![&parent];
        let mut more_parents = self.get_all_parents(&parent.name, gr_map);
        ch_vec.append(&mut more_parents);
        return ch_vec;
    }

    fn find_node_parent<'a>(&self, name: &'a String,
            gr_map: &HashMap<String, (&'a GraphNode, &'a GraphNode)>) -> &'a GraphNode
    {
        //println!("st {}", name);
        //println!("{:?}", gr_map);
        let (_, parent) = gr_map.get(name).unwrap();
        return parent;
    }

    fn build_graph_hash(&self) -> HashMap<String, (&GraphNode, &GraphNode)> {
        let mut map = HashMap::new();
        let nodes = self.root.enum_nodes();
        for (nm, node, parent) in nodes {
            map.insert(nm, (node, parent));
        }
        return map;
    }
}

impl fmt::Display for Graph {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Graph::fmt_graph_line(&self.root, f, 0)
    }
}




fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let orbit_data = read_strs(&filename, ')');

    let orbit_graph = Graph::new(&orbit_data);
    println!("Graph: {}", orbit_graph);

    println!("Direct and indirect orbit count: {}", orbit_graph.count_orbits());

    //println!("enum {:?}", orbit_graph.root.enum_nodes());
    //println!("You: {:?}", orbit_graph.find_node(&String::from("YOU")));
    println!("Dist: {:?}", orbit_graph.find_dist_btwn(&String::from("YOU"), &String::from("SAN")));
}
