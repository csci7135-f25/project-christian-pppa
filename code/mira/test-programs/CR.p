type Conf = (id: int, succ:Node);
event eInit: Conf;
event eFwd: int;

machine Node {
    var id: int;
    var succ: Node; 
    start state Init {
        on eInit do (c:Conf) {
            id = c.id; 
            succ = c.succ; 
            send succ, eFwd, id; 
        }

        on eFwd do (v: int) {
            if (v >= id) {
                send succ, eFwd, v;
            }
        }
    }
}