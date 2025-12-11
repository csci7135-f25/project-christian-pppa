type Conf = (b:B, c:C);

event eInit: Conf;
event eDone; 
event eInitial: A; 
event eWork;
event eSignal;
event eInitialize;

machine A {
  var worker: C; 
  var barrier: B;
  start state Init {
      on eInit do (c:Conf) {
        worker = c.c; 
        barrier = c.b;
        send worker, eWork;
    }

    on eDone do {
        send barrier, eSignal;
    }
  }
}

machine C {
    var boss:A;
    start state Init {
        on eInitial do (a: A) {
            boss = a;
        }
        on eWork do {
            send boss, eDone;
        }
    }
}


machine B {
    var d: int; 
    start state Init {
        on eInitialize do {
            d = 0;
        }
        on eSignal do {
            d = 1;
        }
    }
}