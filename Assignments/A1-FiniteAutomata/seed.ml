let looped =  {states = ["q0";"q1";"q2";"q3";"q4"];
           start = "q0";
           tf = [("q0", 'a', "q1");("q0", 'b', "q2");
                 ("q1", 'b', "q1");("q2", 'a', "q1");
                 ("q2", 'b', "q3");("q2", 'c', "q3")];
           final = ["q3"]
          }
