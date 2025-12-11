type Nodes = seq[Node];
type Init = (id:int, nodes: Nodes, db: DB);
event eInitialize : Init;

event eVote : int;
event eLeader : int;
event eInit;
event eDeclare:Node; 
event eCommit:int; 
event eProp: int; 
event eResp;
event eFinal;

machine Node {
    var my_id: int;
    var votes: int; var received: int; 
    var leader: int;
    var nodes: Nodes;
    var stored: int; var potential: int;
    var controller: Node;
    var has_lead: bool; 
    var db: DB;

    start state Init {

        on eInitialize do (inp: Init) {
            var payload : int;
            var node : Node;
            my_id = inp.id; 
            payload = my_id;
            has_lead = false;
            nodes = inp.nodes;
            db = inp.db;
            votes = 0;
            leader = 0; received = 0; stored = 0; potential = 0;
            foreach (node in nodes){
                send node, eVote, payload;
            }
        }
    
        on eVote do (inp:int) {
            var node : Node;
            if (inp == my_id) {
                votes = votes + 1;
            } 
            if (votes >= sizeof(nodes)/2 +1) {
                foreach(node in nodes){
                    send node, eLeader, my_id;
                }
            }    
        }
        on eLeader do (inp:int) {
            var node: Node;
            leader = inp;
            if (inp == my_id){
                foreach (node in nodes){
                   send node, eDeclare, this;
                }
                send db, eDeclare, this;
            }
        }
        on eDeclare do (n:Node) {
            controller = n;
            has_lead = true; 
        }

        on eCommit do (v:int) {
            var node : Node;
            if (leader == my_id){
                // start two-phase commit 
                foreach(node in nodes) {
                    send node, eProp,v; 
                }   
            }
        }

        on eProp do (v:int) {
            if (has_lead) {
                if (stored == 0){
                    potential = v;
                    send controller, eResp;
                }
            }
        }

        on eResp do {
            var node: Node;
            received = received + 1; 
            if (received == sizeof(nodes)) {
                foreach(node in nodes){
                    send node, eFinal;
                }
            }
        }

        on eFinal do {
            stored = potential;
        }
    }
}

machine DB {
    var sent: bool;
    start state Init {
        on eInit do {
            sent = false;
        }
        on eDeclare  do (l:Node){
            send l, eCommit, 5; 
        }
    }
}