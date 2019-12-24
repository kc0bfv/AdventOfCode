use std::env::args;
use std::collections::VecDeque;
use std::convert::TryInto;

pub mod file_help;
pub mod new_intcode_compy;

use file_help::read_ints_comma_sep;
use new_intcode_compy::{IntcodeThread};

fn main() {
    let filename = args().nth(1).expect("Supply a filename!");
    let input_prog = read_ints_comma_sep(&filename).into_iter().nth(0).unwrap();

    let thread_count = 50;
    let idle_threshold = 1000;

    let mut threads: Vec<IntcodeThread> = vec![];
    let mut thread_out_queue: Vec<VecDeque<i64>> = vec![];
    for thread_ind in 0..thread_count {
        println!("Starting thread {}", thread_ind);
        let new_thread = IntcodeThread::new(thread_ind, &input_prog, Some(-1));
        new_thread.send_thread_data(thread_ind.try_into().unwrap())
                .expect("Sending initial computer number");
        threads.push(new_thread);
        thread_out_queue.push(VecDeque::new());
    }

    let mut prev_idle_x_y: (i64, i64) = (0, 0);
    let mut idle_x_y: (i64, i64) = (0, 0);

    let mut wait_before_resend = true;

    let mut thread_alive: bool = true;
    while thread_alive {
        thread_alive = false;
        //println!("Doing main loop");
        // Do receives from all threads
        for (sending_thread_ind, thread) in threads.iter_mut().enumerate() {
            if thread.check_finished() {
                continue;
            }
            thread_alive = true;
            match thread.recv_data_from_thread() {
                Ok(val) => {
                        if sending_thread_ind == 0 {
                            wait_before_resend = false;
                        }
                        if val < 0 {
                            panic!("Got negative address val...");
                        }
                        let addr: usize = i64::try_into(val).unwrap();
                        let x = thread.recv_data_from_thread_block()
                                .expect("Fail in getting x");
                        let y = thread.recv_data_from_thread_block()
                                .expect("Fail in getting y");
                        if addr == 255 {
                            idle_x_y = (x, y);
                            println!("Got 255 {} {}", x, y);
                            continue;
                        }
                        else if addr > thread_count {
                            panic!("Val for out-of-bounds addr: {} x {} y {}",
                                    addr, x, y);
                        }
                        let toq: &mut VecDeque<i64> = thread_out_queue
                                .get_mut(addr)
                                .expect(&format!("Getting thread_out_queue {}",
                                        addr));
                        println!("Received {} {} for {}", x, y, addr);
                        toq.push_back(x);
                        toq.push_back(y);
                        ();
                    },
                Err(_) => (),
            };
        }

        // Check idle queues
        let is_idle: Vec<i64> = threads.iter_mut()
                .map(|thread| thread.check_idle_count())
                .collect();
        if is_idle.iter().all(|val| *val > idle_threshold) && !wait_before_resend{
            if idle_x_y.1 == prev_idle_x_y.1 {
                panic!("Part 2: x {} y {}", idle_x_y.0,
                        idle_x_y.1);
            }
            prev_idle_x_y = idle_x_y;
            println!("Found idle state");
            thread_out_queue[0].push_back(idle_x_y.0);
            thread_out_queue[0].push_back(idle_x_y.1);
            wait_before_resend = true;
        }

        // Do any necessary sends to all threads
        for (thread_ind, thread) in threads.iter_mut().enumerate() {
            if thread.check_finished()
                    || thread_out_queue[thread_ind].len() == 0 {
                continue;
            }
            while thread_out_queue[thread_ind].len() > 0 {
                thread.send_thread_data(thread_out_queue[thread_ind]
                        .pop_front()
                        .expect("In sending queue")
                    )
                    .expect("Error sending data");
            }
        }
    }

}
