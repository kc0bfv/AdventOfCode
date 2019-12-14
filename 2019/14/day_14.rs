use std::env::args;
use std::collections::HashMap;

pub mod file_help;
use file_help::read_strs;

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct Ingred {
    amt: u64,
    chem: String,
}
impl Ingred {
    fn new(to_parse: &str) -> Ingred {
        let stripped = to_parse.trim();
        let splitted: Vec<&str> = stripped.split(' ').collect();
        if splitted.len() != 2 {
            panic!("Val split incorrectly {}", to_parse);
        }
        let chem: String = splitted[1].chars().filter(|char| char.is_alphabetic()).collect();
        let amt: u64 = splitted[0].parse().unwrap();
        Ingred { amt, chem }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct Formula {
    target: Ingred,
    sources: Vec<Ingred>,
}
impl Formula {
    fn new(target: Ingred, sources: Vec<Ingred>) -> Formula {
        Formula { target, sources }
    }
}

#[derive(Debug, Hash)]
struct GraphNode {
    ingred: Ingred, // the ingredient rep'd by this node
    reqd_by: Vec<String>, // chemical names that require this ingred
    requires: Vec<String>, // chemical names that this ingred requires

    is_satisfied: bool,
    reqd_amt: u64,
    act_prod: u64,
    runs_needed: u64,
}
// This graph works with all nodes pointing at what requires them
// So, fuel is required by nothing
// The things that fuel needs are required by fuel, working backwards to ore
// is_satisfied needs to get set to true when everything that requires a node is satisfied
// When satisfying an ingred, mark the nodes it requires with the amt required (add it on to the existing) and actual amount that gets produced (gotta be a multiple of what's possible)
// When ore is satisfied, we know how to get from fuel to ore and how much is required
impl GraphNode {
    fn new(formula: &Formula) -> GraphNode {
        let requires: Vec<String> = formula.sources.iter().map(|ingred| ingred.chem.clone()).collect();
        GraphNode {
                ingred: formula.target.clone(),
                reqd_by: Vec::<String>::new(),
                requires: requires,

                is_satisfied: false,
                reqd_amt: 0,
                act_prod: 0,
                runs_needed: 0,
            }
    }
}

fn parse_line(line: Vec<&str>) -> Formula {
    if line.len() != 2 {
        println!("Invalid line parse! {:?}", line);
    }
    let target = Ingred::new(&line[1]);

    let sources = line[0].split(',').map(|part| Ingred::new(part)).collect();

    return Formula::new(target, sources);
}

fn build_data_structs_with_reqd(formulas: &Vec<Formula>, fuel_need: u64)
        -> (HashMap<String, Formula>, HashMap<String, GraphNode>)
{
    let mut name_form_map: HashMap<String, Formula> = HashMap::new();
    formulas.into_iter().for_each(|formula| {
                if name_form_map.contains_key(&formula.target.chem) {
                    panic!("Hashmap already contains key {}", formula.target.chem);
                }
                name_form_map.insert(formula.target.chem.clone(), (*formula).clone());
            });

    let mut name_node_map: HashMap<String, GraphNode> = HashMap::new();
    // First the nodes get the ingred and requires's filled out
    name_form_map.keys().for_each(|key| {
                let form: &Formula = name_form_map.get(key).unwrap();
                let node: GraphNode = GraphNode::new(form);
                name_node_map.insert(key.clone(), node);
            });
    // Next they need their required by filled out
    recurs_fill_name_node_map_reqd_by("FUEL".to_string(), None, &mut name_node_map);

    // Now set the amount of fuel needed
    {
        let fuel_node = name_node_map.get_mut("FUEL").unwrap();
        fuel_node.act_prod = fuel_need;
        fuel_node.reqd_amt = fuel_need;
        fuel_node.runs_needed = fuel_need;
    }

    // Next they need their reqd_amt act_prod and is_satisfied filled out
    recurs_calc_amts("FUEL".to_string(), &mut name_node_map, &name_form_map);

    // Now - ORE should be satisfied, and calculated and ready for output

    return (name_form_map, name_node_map);
}

fn recurs_fill_name_node_map_reqd_by(chem: String, from: Option<String>, nnm: &mut HashMap<String, GraphNode>) {
    /*
    if chem == "ORE".to_string() {
        return;
    }
    */
    {
        let node: &mut GraphNode = match nnm.get_mut(&chem) {
                Some(node) => node,
                None => panic!("Chem not found in nnm {}", chem),
            };
        match from {
            Some(reqd_by) => {
                    if node.reqd_by.contains(&reqd_by) {
                        //panic!("Node already has reqd_by! {} {}", chem, reqd_by);
                        // I think that isn't actually a problem, and in this case
                        // we don't want to recurse again...
                        return;
                    } else {
                        node.reqd_by.push(reqd_by);
                    }
                }
            None => (),
        };
    }
    let requires: Vec<String> = nnm.get(&chem).unwrap().requires.clone();
    requires.iter().for_each(|req_chem| recurs_fill_name_node_map_reqd_by(req_chem.to_string(), Some(chem.clone()), nnm));
}

fn is_it_satisfied(chem: String, nnm: &mut HashMap<String, GraphNode>) -> bool {
    let is_satisfied;
    {
        let node: &GraphNode = match nnm.get(&chem) {
                Some(node) => node,
                None => panic!("Chem not found in nnm in satisfied! {}", chem),
            };
        if node.is_satisfied { return true; }
        is_satisfied = node.reqd_by.iter().all(|reqing_name| {
                let reqing_node = match nnm.get(reqing_name) {
                        Some(node) => node,
                        None => panic!("Req not found in nnm satisfied {} {}", chem, reqing_name),
                    };
                reqing_node.is_satisfied.clone()
            });
    }
    let node: &mut GraphNode = match nnm.get_mut(&chem) {
            Some(node) => node,
            None => panic!("Chem not found in nnm in satisfied! {}", chem),
        };
    node.is_satisfied = is_satisfied;
    return node.is_satisfied;
}

fn sum_reqd_amts(chem: String, nnm: &mut HashMap<String, GraphNode>, nfm: &HashMap<String, Formula>) {
    let req_amt: u64;
    // Nodes not in map should have been found a couple times now...  so
    // unwrap is less terrible
    {
        let node: &GraphNode = nnm.get(&chem).unwrap();
        req_amt = node.reqd_by.iter().map(|reqing_name| {
                let reqing_node: &GraphNode = nnm.get(reqing_name).unwrap();
                let reqing_formula: &Formula = nfm.get(reqing_name).unwrap();

                // Get the current ingredient out of the requiring formula
                let ingred_in_form_vec: Vec<&Ingred> = reqing_formula.sources.iter().filter(|ingred| ingred.chem == chem).collect();
                if ingred_in_form_vec.len() > 1 { panic!("Found too many ingreds matching! {}", chem); };
                // This is the amount of the ingredient needed to produce the formula
                let amt_in_form_ingred = ingred_in_form_vec[0].amt.clone();
                
                // This is the amount of the ingredient needed to produces the actually-produced amount of requiring formula result
                amt_in_form_ingred * reqing_node.runs_needed
            }).sum();
    }

    let form: &Formula = nfm.get(&chem).unwrap();
    let target_mult: u64 = form.target.amt.clone();
    let runs_needed: u64 = div_int_round_up(req_amt, target_mult);
    let act_prod: u64 = runs_needed * target_mult;

    let node: &mut GraphNode = nnm.get_mut(&chem).unwrap();
    node.act_prod = act_prod;
    node.reqd_amt = req_amt;
    node.runs_needed = runs_needed;
}

fn div_int_round_up(top: u64, bot: u64) -> u64 {
    (top + (bot-1)) / bot
}

fn recurs_calc_amts(chem: String, nnm: &mut HashMap<String, GraphNode>, nfm: &HashMap<String, Formula>) {
    // Is the current one satisfied?  If not return.  It should get recursed to a separate way when it is satisfied
    if ! is_it_satisfied(chem.clone(), nnm) { return; }

    /*
    let node: &mut GraphNode = match nnm.get_mut(&chem) {
            Some(node) => node,
            None => panic!("Chem not found in calc amts {}", chem),
        };
    */

    // If it was satisfied, use the actual produced amounts on the reqd_by's, and the formulas, to sum up how much is required here, then calculate the actual production
    if chem != "FUEL".to_string() {
        sum_reqd_amts(chem.clone(), nnm, nfm);
    }

    // If it was satisfied now recurse into all the requires nodes if any - ore wont have any so it'll return
    let requires: Vec<String> = nnm.get(&chem).unwrap().requires.clone();
    requires.iter().for_each(|req_chem| recurs_calc_amts(req_chem.to_string(), nnm, nfm));
}

fn main() {
    let filename: String = args().skip(1).take(1).collect();
    let input_lines: Vec<Vec<String>> = read_strs(&filename, '!');
    let input_lines_split: Vec<Vec<&str>> = input_lines.iter().map(|line| line[0].split("=>").collect()).collect();

    let mut formulas: Vec<Formula> = input_lines_split.into_iter().map(|line| parse_line(line)).collect();
    let ore_form = Formula::new(Ingred::new("1 ORE"), vec![]);
    formulas.push(ore_form);
    //formulas.iter().for_each(|formula| println!("Tgt {:?} Srcs {:?}", formula.target, formula.sources));
    let (_, name_node_map) = build_data_structs_with_reqd(&formulas, 1);
    //println!("Map {:?}", name_node_map);
    let ore_node = name_node_map.get("ORE").unwrap();
    println!("Part 1 - required ore: {}", ore_node.act_prod);

    let part_2_try_fuel_amt: u64 = args().skip(2).take(1).next().unwrap().parse().unwrap();
    let (_, name_node_map) = build_data_structs_with_reqd(&formulas, part_2_try_fuel_amt);
    let ore_node = name_node_map.get("ORE").unwrap();
    println!("Part 2 - required ore: {} {}", ore_node.act_prod, 
        {if ore_node.act_prod > 1000000000000 { "GOT IT" } else { "NOT YET" } });
}
