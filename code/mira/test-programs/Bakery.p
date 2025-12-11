type Procs = seq[Proc];
type Init = (procs:Procs,baker:Baker);
type Serve = (ticket: int, sender: Proc);

event eInitialize: Init;
event eInitial;
event eChooseQuery: Proc;
event eChooseQueryResp: bool;
event eNumberQuery: Proc;
event eNumberQueryResp: int;
event eServe: Serve;
event eGrant: int;

machine Proc {
    var id: int;
    var procs: Procs;
    var baker: Baker;
    var grant: int;

    var choosingResponses: int;
    var allChoosingFalse: bool;
    var numberResponses: int;
    var maxNumber: int;
    var myTicket: int;
    var choosing: bool;

    start state Init {
        on eInitialize do (inp: Init){
            var p: Proc;
            choosingResponses = 0;
            allChoosingFalse = true;
            numberResponses = 0;
            maxNumber = 0;
            choosing = false;
            baker = inp.baker;
            procs = inp.procs;
            grant = 0;
            myTicket = 0;
            foreach (p in procs) {
                send p, eChooseQuery, this;
            }
        }

        on eChooseQueryResp do (resp: bool) {
            var p: Proc;
            choosingResponses = choosingResponses + 1;
            if (resp) {
                allChoosingFalse = false;
            }
            if (choosingResponses == sizeof(procs)) {
                if (allChoosingFalse) {
                    choosingResponses = 0;
                    numberResponses = 0;
                    maxNumber = 0;
                    choosing = true;
                    foreach (p in procs) {
                        send p, eNumberQuery, this;
                    }
                } 
            }
        }
        on eNumberQueryResp do (num: int) {
            numberResponses = numberResponses + 1;
            if (num > maxNumber) {
                maxNumber = num;
            }

            if (numberResponses == sizeof(procs)) {
                myTicket = maxNumber + 1;
                choosing = true;
                send baker, eServe, (ticket=myTicket,sender=this);
            }
        }

        on eChooseQuery do (sender:Proc) {
            send sender, eChooseQueryResp, choosing;
        }

        on eNumberQuery do  (sender:Proc) {
            send sender, eNumberQueryResp, myTicket;
        }

        on eGrant do (ticket: int) {
            grant = ticket;
        }
    }
}

machine Baker {
    var counter: int;
    var current: int;
    start state Init {
        on eInitial do {
            current = 0;
            counter = 0;
        }

        on eServe do (v:Serve) {
            if (v.ticket == current){
                send v.sender,eGrant, v.ticket;
            }

            current = current + 1;
        }
     }
}