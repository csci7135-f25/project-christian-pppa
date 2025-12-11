type Nodes = seq[Node];
type Init = (id:int, nodes: Nodes);
event eInitialize : Init;

event eVote : int;
event eLeader : int;

machine Node {
    var my_id: int;
    var votes: int; 
    var leader: int;
    var nodes: Nodes;

    start state Init {

        on eInitialize do (inp: Init) {
            var payload : int;
            var node : Node;
            my_id = inp.id; 
            payload = my_id;
            nodes = inp.nodes;
            votes = 0;
            leader = 0;
            foreach (node in nodes){
                send node, eVote, payload;
            }
        }
    
        on eVote do (inp:int) {
            var node : Node;
            if (inp == my_id) {
                votes = votes + 1;
            } 
            if (votes >= sizeof(nodes)/2 +1)
                foreach(node in nodes){
                    send node, eLeader, my_id;
                }
            }
        on eLeader do (inp:int) {
            leader = inp;
        }
    }
}
