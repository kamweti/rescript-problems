module Prelude = {
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
            | x if x > mylist->Prelude.length => None
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
    let length = list => list->Prelude.length
}

module Five = {
    let reverse = list => list->Prelude.reverse
}

module Six = {
    let isReverse = (subject, ~claim) => 
        subject->Prelude.reverse->Belt.List.eq(claim, (x, y) => x == y)
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