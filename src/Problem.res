module Prelude = {
    module List = {
        let reverse = list => {

            let rec loop = (list, reversed_list) => switch list {
            | list{} => reversed_list
            | list{head, ...rest} => loop(rest, list{head, ...reversed_list})
            }

            loop(list, list{})
        }

        let length = list => {
            
            let rec loop = (list, length) => switch list {
            | list{} => length
            | list{_, ...rest} => loop(rest, length + 1)
            }

            loop(list, 0)
        }
    }
}

module One = {
    let rec last = mylist => {
        switch mylist {
        | list{} => None
        | list{x} => Some(x)
        | list{_, ...rest} => last(rest)
        }
    }
}

module Two = {
    let rec lasttwo = mylist => {
        switch mylist {
        | list{} => None
        | list{x, y} => Some(x, y)
        | list{_, ...rest} => lasttwo(rest)
        }
    }
}
module Three = {
    let rec nth = (mylist, index) => {
        switch index {
            | x if (x < 0) => None
            | x if x > mylist->Prelude.List.length => None
            | x if x == 0 => mylist->Belt.List.head
            | _ => {
                switch mylist {
                | list{} => None
                | list{_, ...rest} => nth(rest, index -1)
                }
            }
        }
    }
}

module Four = {
    let length = list => list->Prelude.List.length
}

module Five = {
    let reverse = list => list->Prelude.List.reverse
}

module Six = {
    let isReverse = (subject, ~claim) => 
        subject->Prelude.List.reverse->Belt.List.eq(claim, (x, y) => x == y)
}

module Seven = {
    let flatten = tree => {
        let rec loop = (t, acc) =>
            switch t {
            | list{} => acc
            | list{head, ...tail} => loop(tail, acc->Belt.List.concat(head))
            }

            loop(tree, list{})
    }
}

module Eight = {
    let rec uniquify = dupe_list =>
        switch dupe_list {
        | list{x, y, ...rest} => 
            switch (x == y) {
            | true => uniquify(list{x, ...rest})
            | false => list{x, ...uniquify(list{y, ...rest})}
            }
        | _ as z => z
        }
}

module Nine = {
    let pack = dupe_list => {

        let rec loop = (dupe_list, current, acc) => {
            switch dupe_list {
            | list{} => acc
            | list{x} => list{list{x, ...current}, ...acc}
            | list{x, y, ...rest} => 
                switch (x == y) {
                | true => {

                    loop(
                        list{y, ...rest},
                        list{x, ...current},
                        acc
                    )
                }
                | false => 
                    loop(
                        list{y, ...rest},
                        list{},
                        list{list{x, ...current}, ...acc}
                    )
                }
            }
        }

        loop(dupe_list, list{}, list{})->Prelude.List.reverse
    }
}

module Ten = {
    let run_length_encode = list => {
        let rec loop = (l, counter, acc) => {

            switch l {
            | list{} => acc
            | list{x} => list{(counter + 1, x), ...acc}
            | list{x, y, ...rest} => {

                switch (x == y) {
                | true => {

                    loop(
                        list{y, ...rest},
                        counter + 1,
                        acc
                    )
                }
                | false => 
                    loop(
                        list{y, ...rest},
                        0,
                        list{(counter + 1, x), ...acc}
                    )
                }
            }
            }
        }

        loop(list, 0, list{})->Prelude.List.reverse
    }
}

module Eleven = {
    // if an element has no duplicates, it is simply copied into the result list.
    // since ReScript lists contain elements of the same type
    // we need to define a type to hold both single & multiple value lists

    exception InvalidCount
    type list_length<'a> = 
        | One('a)
        | Many(int, 'a)

    let run_length_encode_modified = list => 
        Ten.run_length_encode(list)->List.map(tuple => 
            switch tuple {
            | (1, x) => One(x)
            | (count, x) if (count > 1) => Many(count, x)
            | _ => raise(InvalidCount)
            }
        )
}

module Twelve = {
    open Eleven
    
    let decode_run_length = encoded_list => {
        let rec loop = (list, acc) => {
            switch list {
            | list{} => acc
            | list{head, ...rest} => 
                switch head {
                | One(x) => loop(rest, list{x, ...acc})
                | Many(count, x) => loop(rest, Belt.List.make(count, x)->Belt.List.concat(acc))
                }
            }
        }

        loop(encoded_list, list{})->Belt.List.reverse
    }
}