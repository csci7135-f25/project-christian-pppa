    type Pars = seq[Participant];
    type CoordInit = Pars;
    event eInitialize: CoordInit;
    event eInit: Coordinator;
    event eProp: int;
    event eFinal;
    event eResp;

    machine Coordinator {
        var nodes: Pars;
        var received: int;
        var acc: bool;
        start state Init {
            on eInitialize do (i:CoordInit) {
                var par: Participant;
                nodes = i; 
                received = 0;
                foreach (par in nodes) {
                send par, eProp, 5;
                }
            }
            on eResp do {
                var par: Participant;
                received = received + 1;
                if (received >= sizeof(nodes)){
                    foreach (par in nodes){
                        send par, eFinal;
                    }
                }
            }
        }
    }

    machine Participant {
        var internal: int;
        var potential: int;
        var coord: Coordinator;
        start state Init {
            on eInit do (c: Coordinator) {
                internal = 0;
                coord = c; 
                potential = 0;
            }
            on eProp do (z : int) {
                potential = z;
                if (internal == 0){
                    send coord, eResp;
                }
            }
            on eFinal do {
                internal = potential;
            }
        }
    }   