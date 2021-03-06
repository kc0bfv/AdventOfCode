use std::thread;
use std::sync::mpsc;
use std::collections::HashMap;
use std::convert::TryInto;
//use std::io;


/*
fn print_state(init: &str, items: &Vec<i64>) {
    println!("{}", init);
    items.iter().map(|item| print!("{},", item)).count();
    println!("");
    ()
}
*/

/*
fn get_int_from_stdin() -> i64 {
    let mut inval = String::new();
    io::stdin().read_line(&mut inval).unwrap();
    match inval.trim().parse::<i64>() {
        Ok(val) => val,
        Err(err) => panic!("Error on input: {}", err),
    }
}
*/

struct MemRep {
    memory_n: HashMap<usize, i64>,
    cur_pos: usize,
    rel_base: usize,
}
impl MemRep {
    fn new(memory: Vec<i64>, cur_pos: usize, rel_base: usize) -> Self {
        let mut memory_n = HashMap::<usize, i64>::new();
        memory.iter().enumerate().for_each(|(ind, val)| { memory_n.insert(ind, *val); });
        Self{ memory_n, cur_pos, rel_base }
    }
    fn get_mem(&self, ind: usize) -> i64 {
        match self.memory_n.get(&ind) {
            Some(val) => *val,
            None => 0
        }
    }
    fn set_mem(&mut self, ind: usize, val: i64) {
        self.memory_n.insert(ind, val);
    }
}

#[derive(Debug)]
enum ParamModes {
    Pos, Imm, Rel
}

fn get_param_modes(opcode: i64, count: usize) -> Vec<ParamModes> {
    if opcode < 0 {
        panic!("Negative opcode found!");
    }
    let mode_sec = opcode / 100;
    let base: i64 = 10;
    let modes: Vec<ParamModes> = (0..count).map(|param_ind| 
            {
                let mode = (mode_sec /
                        base.checked_pow(param_ind as u32).unwrap()) % 10;
                match mode {
                    0 => ParamModes::Pos,
                    1 => ParamModes::Imm,
                    2 => ParamModes::Rel,
                    _ => panic!("Invalid mode! pos {} mode {} opcode {}",
                            count, mode, opcode),
                }
            }
        ).collect();
    //println!("Modes {:?}", modes);
    modes
}

fn do_4_op<F>(opcode: i64, mut memrep: &mut MemRep, mut func: F )
    where F: FnMut(i64, i64) -> i64
{
    let modes = get_param_modes(opcode, 3);
    let val_1: i64 = handle_read_param(&mut memrep, 1, &modes[0]);
    let val_2: i64 = handle_read_param(&mut memrep, 2, &modes[1]);
    let out_pos: usize = resolve_param_pos(&mut memrep, 3, &modes[2]);
    //println!("4 op {} {} {}", val_1, val_2, out_pos);
    memrep.set_mem(out_pos, func(val_1, val_2));
    memrep.cur_pos += 4;
}

fn do_input<IO>(_prgm_index: usize, opcode: i64, mut memrep: &mut MemRep, io_handler: &mut IO) -> ()
    where IO: Iterator<Item = i64> + OutputHandler,
{
    let modes = get_param_modes(opcode, 1);
    let out_pos: usize = resolve_param_pos(&mut memrep, 1, &modes[0]);
    let in_val = match io_handler.next() {
            Some(val) => val,
            None => panic!("Iterator had no inputs remaining"),
        };
    memrep.set_mem(out_pos, in_val);
    memrep.cur_pos += 2;
}

fn handle_read_param(mut memrep: &mut MemRep, param_ind: usize, mode: &ParamModes) -> i64 {
    match mode {
        ParamModes::Imm => memrep.get_mem(memrep.cur_pos + param_ind),
        ParamModes::Pos => {
                let pos = resolve_param_pos(&mut memrep, param_ind, mode);
                memrep.get_mem(pos)
            }
        ParamModes::Rel => {
                let pos = resolve_param_pos(&mut memrep, param_ind, mode);
                memrep.get_mem(pos)
            }
        //_ => panic!("Invalid parameter mode at read time!"),
    }
}

fn resolve_param_pos(memrep: &mut MemRep, param_ind: usize,
        mode: &ParamModes) -> usize
{
    let param = memrep.get_mem(memrep.cur_pos + param_ind);
    match mode {
            ParamModes::Pos => param as usize,
            ParamModes::Imm => panic!("Immediate mode not supported in output!"),
            ParamModes::Rel => {
                    let pos_i64: i64 = param + (memrep.rel_base as i64);
                    if pos_i64 < 0 {
                        panic!("Trying to use negative memory position!");
                    }
                    pos_i64 as usize
                },
        }
}

fn do_output<IO>(_prgm_index: usize, opcode: i64, memrep: &mut MemRep, out_vals: &mut Vec<i64>, io_handler: &mut IO)
    where IO: Iterator<Item = i64> + OutputHandler,
{
    let modes = get_param_modes(opcode, 1);
    let out_val: i64 = handle_read_param(memrep, 1, &modes[0]);
    out_vals.push(out_val);
    match io_handler.output(out_val) {
        Err(err) => panic!("Failed writing output! {}", err),
        _ => ()
    }
    memrep.cur_pos += 2;
}

fn jump_test<F>(opcode: i64, memrep: &mut MemRep,
        mut tester: F)
    where F: FnMut(i64) -> bool
{
    let modes = get_param_modes(opcode, 2);
    let cond: i64 = handle_read_param(memrep, 1, &modes[0]);
    let dest: i64 = handle_read_param(memrep, 2, &modes[1]);
    if tester(cond) {
        memrep.cur_pos = dest as usize;
    } else {
        memrep.cur_pos += 3;
    }
}
fn rebase_rel(opcode: i64, memrep: &mut MemRep)
{
    let modes = get_param_modes(opcode, 1);
    let adj: i64 = handle_read_param(memrep, 1, &modes[0]);
    let new_base = (memrep.rel_base as i64) + adj;
    if new_base < 0 {
        panic!("Invalid rel_base calculated: {}", new_base);
    }
    memrep.rel_base = new_base as usize;
    memrep.cur_pos += 2;
}

pub trait OutputHandler {
    fn output(&mut self, val: i64) -> Result<(), &'static str>;
}

pub fn run_program<IO>(prgm_index: usize, initial_memory: &Vec<i64>,
        io_handler: &mut IO) -> (Vec<i64>, Vec<i64>)
    where IO: Iterator<Item = i64> + OutputHandler,
{
    let ret = run_program_inst_cnt(prgm_index, initial_memory,
            io_handler);
    (ret.0, ret.1)
}

pub fn run_program_inst_cnt<IO>(prgm_index: usize, initial_memory: &Vec<i64>,
        io_handler: &mut IO) -> (Vec<i64>, Vec<i64>, usize)
    where IO: Iterator<Item = i64> + OutputHandler,
{
    let mut memrep = MemRep::new(initial_memory.to_vec(), 0, 0);
    let mut out_vals: Vec<i64> = Vec::new();
    let mut inst_count: usize = 0;

    while memrep.cur_pos < initial_memory.len() {
        let opcode = memrep.get_mem(memrep.cur_pos);
        inst_count += 1;
        //println!("Execing {}", opcode);
        match opcode % 100 {
            1 => do_4_op(opcode, &mut memrep, std::ops::Add::add),
            2 => do_4_op(opcode, &mut memrep, std::ops::Mul::mul),
            3 => do_input(prgm_index, opcode, &mut memrep, io_handler),
            4 => do_output(prgm_index, opcode, &mut memrep, &mut out_vals, io_handler),
            5 => jump_test(opcode, &mut memrep, |val| val != 0),
            6 => jump_test(opcode, &mut memrep, |val| val == 0),
            7 => do_4_op(opcode, &mut memrep, |val_1, val_2| {
                    if val_1 < val_2 {
                        1
                    } else {
                        0
                    }
                }),
            8 => do_4_op(opcode, &mut memrep, |val_1, val_2| {
                    if val_1 == val_2 {
                        1
                    } else {
                        0
                    }
                }),
            9 => rebase_rel(opcode, &mut memrep),
            99 => {
                    //cur_pos += 1;
                    break;
                },
            _ => panic!("Unknown opcode encountered! {}", opcode),
        }
        //print_state("Current state", &memory);
        //println!("Now at position {}", cur_pos);
    }
    let ret_mem = (0..initial_memory.len()).map(|ind| memrep.get_mem(ind)).collect();
    return (ret_mem, out_vals, inst_count);
}

#[derive(Debug)]
struct IntcodeThreadIOHandler {
    input_get:   mpsc::Receiver<i64>,
    output_send: mpsc::Sender<i64>,
    no_recv_output: Option<i64>, 
    thread_index: usize,

    idle_count: usize,
    idle_sender: mpsc::Sender<i64>,
}
// no_recv_output - None (blocking wait for input), Some(val)
//   (val to send when no recv data is available and it would block)
impl IntcodeThreadIOHandler {
    fn new(thread_index: usize,
            input_get: mpsc::Receiver<i64>, output_send: mpsc::Sender<i64>,
            no_recv_output: Option<i64>,

            idle_sender: mpsc::Sender<i64>)
        -> Self
    {
        Self { input_get, output_send, no_recv_output, thread_index,
                idle_count: 0, idle_sender }
    }

    fn send_idle(&self) {
        self.idle_sender.send(self.idle_count.try_into()
                .expect("Idle conversion")).expect("Sending idle");
    }
}
impl Iterator for IntcodeThreadIOHandler {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        match self.no_recv_output {
            None => match self.input_get.recv() {
                    Ok(val) => Some(val),
                    Err(_) => {
                            println!("Thread {} error during input.",
                                    self.thread_index);
                            None
                        },
                },
            Some(backup_val) => match self.input_get.try_recv() {
                    Ok(val) => {
                            self.idle_count = 0;
                            self.send_idle();
                            Some(val)
                        },
                    Err(_) => {
                            self.idle_count += 1;
                            self.send_idle();
                            Some(backup_val)
                        },
                },
        }
        //self.input_get.next()
    }
}
impl OutputHandler for IntcodeThreadIOHandler {
    fn output(&mut self, val: i64) -> Result<(), &'static str> {
        if !self.no_recv_output.is_none() {
            self.idle_count = 0;
            self.send_idle();
        }
        match self.output_send.send(val) {
            Ok(val) => Ok(val),
            Err(_) => Err("Send error"),
        }
    }
}

#[derive(Debug)]
pub struct IntcodeThread {
    thread: thread::JoinHandle<()>,
    thread_index: usize,
    send_to_thread_mpsc: mpsc::Sender<i64>,
    recv_from_thread_mpsc: mpsc::Receiver<i64>,
    thread_done_mpsc: mpsc::Receiver<bool>,
    reported_finished: bool,

    idle_count: i64,
    idle_count_receiver: mpsc::Receiver<i64>,
}
impl IntcodeThread {
    pub fn new(thread_index: usize, initial_memory: &Vec<i64>,
            no_recv_output: Option<i64>)
        -> Self
    {
        let memory_copy = initial_memory.clone();
        let (input_tx, input_rx) = mpsc::channel();
        let (output_tx, output_rx) = mpsc::channel();
        let (idle_count_tx, idle_count_rx) = mpsc::channel();
        let (finished_tx, finished_rx) = mpsc::channel();

        let handle = thread::spawn(move || {
                let mut thread_io_handler = IntcodeThreadIOHandler::new(
                        thread_index, input_rx, output_tx, no_recv_output,
                        idle_count_tx);
                run_program(thread_index, &memory_copy, &mut thread_io_handler);
                match finished_tx.send(true) {
                    Ok(_) => (),
                    Err(err) => println!("Thread {} error sending finished {}",
                            thread_index, err),
                }
                println!("Thread {} done!", thread_index);
            });

        Self {
            thread: handle,
            thread_index: thread_index,
            send_to_thread_mpsc: input_tx,
            recv_from_thread_mpsc: output_rx,
            thread_done_mpsc: finished_rx,
            reported_finished: false,
            idle_count: 0,
            idle_count_receiver: idle_count_rx,
        }
    }
    pub fn check_finished(&mut self) -> bool {
        if self.reported_finished {
            return true;
        }
        match self.thread_done_mpsc.try_recv() {
            Ok(_) => { self.reported_finished = true; true },
            Err(_) => false,
        }
    }
    pub fn send_thread_data(&self, val: i64) -> Result<(), mpsc::SendError<i64>> {
        self.send_to_thread_mpsc.send(val)
    }
    pub fn recv_data_from_thread(&self) -> Result<i64, mpsc::TryRecvError> {
        self.recv_from_thread_mpsc.try_recv()
    }
    pub fn recv_data_from_thread_block(&self) -> Result<i64, mpsc::RecvError> {
        self.recv_from_thread_mpsc.recv()
    }
    pub fn check_idle_count(&mut self) -> i64 {
        let mut got_something = true;
        while got_something {
            match self.idle_count_receiver.try_recv() {
                Ok(val) => { self.idle_count = val; },
                Err(_)  => { got_something = false; },
            };
        }
        self.idle_count
    }
}

/*
pub fn run_program_thread(prgm_index: usize, initial_memory: &Vec<i64>) -> (thread::JoinHandle<()>, mpsc::Sender<i64>, mpsc::Receiver<i64>, mpsc::Receiver<bool>) {
    let (input_tx, input_rx) = mpsc::channel();
    let (output_tx, output_rx) = mpsc::channel();
    let (finished_tx, finished_rx) = mpsc::channel();

    let memory: Vec<i64> = initial_memory.to_vec();


    let handle = thread::spawn(move || {
            let mut input_iter = MPSCRxIter::new(input_rx);
            let output_er = MPSCTx::new(output_tx);
            let output_func = |val| { output_er.output(val) }; 
            run_program(prgm_index, &memory, &mut input_iter, output_func);
            match finished_tx.send(true) {
                Ok(_) => (),
                Err(err) => println!("Error sending finished {}", err),
            }
            println!("Thread {} done!", prgm_index);
        });

    return (handle, input_tx, output_rx, finished_rx);
}
*/

/*
pub fn output_println<T>(value: T) -> Result<(), &'static str>
    where T: std::fmt::Display,
{
    println!("Output {}", value);
    Ok(())
}
*/

/*
struct MPSCTx<T> {
    output_tx: mpsc::Sender<T>,
}
impl<T> MPSCTx<T> {
    fn new(output_tx: mpsc::Sender<T>) -> MPSCTx<T> {
        MPSCTx{ output_tx }
    }
    fn output(&self, value: T) -> Result<(), &'static str>{
        match self.output_tx.send(value) {
            Ok(val) => Ok(val),
            Err(_) => Err("Failed to send"),
        }
    }
}

struct MPSCRxIter<T> {
    input_rx: mpsc::Receiver<T>,
}
impl<T> MPSCRxIter<T> {
    fn new(input_rx: mpsc::Receiver<T>) -> MPSCRxIter<T> {
        MPSCRxIter{ input_rx }
    }
}
impl<T> Iterator for MPSCRxIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        println!("Starting wait for input");
        match self.input_rx.recv() {
            Ok(val) => Some(val),
            Err(_) => { println!("Error during input"); None },
        }
    }
}
*/

/*
fn mpsc_tx_output_setup<T, F>(output_tx: mpsc::Sender<T>) -> Box<F>
    where F: Fn(T) -> Result<T, &'static str>,
{
    let mpsc_tx_output = move |value| {
            match output_tx.send(value) {
                Ok(val) => Ok(val),
                Err(_) => Err("Failed to send"),
            }
        };
    Box::new(mpsc_tx_output)
}
*/
