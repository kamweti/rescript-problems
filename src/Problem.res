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

    module Array = {
        let pickRandom : (array<'a>) => (array<'a>, array<'a>) =
            (stack) => {
                let index = Js.Math.random_int(0, stack->Js.Array.length)
                let drawn = Js.Array.removeCountInPlace(~pos=index, ~count=1, stack)

                (drawn, stack)
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

        loop(encoded_list, list{})->Prelude.List.reverse
    }
}

module Thirteen = {
    open Eleven
    
    let run_length_encode_direct = list => {
        
        let rle = (counter, x) => 
            switch counter {
            | c if c == 1 => One(x)
            | c if c > 1  => Many(c, x)
            | _ => raise(InvalidCount)
            }

        let rec loop = (l, counter, acc) => {
            
            switch l {
            | list{} => acc
            | list{x} => list{rle(counter + 1, x), ...acc}
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
                        list{rle(counter + 1, x), ...acc}
                    )
                }
            }
            }
        }

        loop(list, 0, list{})->Prelude.List.reverse
    }
}

module Fourteen = {
    let duplicate = list => {
        let rec loop = (l, acc) =>
            switch l {
            | list{} => acc
            | list{head, ...rest} => loop(rest, list{head, head, ...acc})
            }
        
        loop(list, list{})->Prelude.List.reverse
    }
}

module Fifteen = {
    let duplicate = (list, times) => {
        let rec loop = (l, acc) =>
            switch l {
            | list{} => acc
            | list{head, ...rest} => loop(rest, Belt.List.make(times, head)->Belt.List.concat(acc))
            }
        
        loop(list, list{})->Prelude.List.reverse
    }
}

module Sixteen = {
    let drop = (list, nth) => {
        let rec loop = (l, counter, acc) =>
            switch l {
            | list{} => acc
            | list{head, ...rest} => 
                let acc = (counter == nth) ? acc : list{head, ...acc}
                loop(rest, (counter + 1), acc)
            }
        
        loop(list, 1, list{})->Prelude.List.reverse
    }
}

module Seventeen = {
    let split = (list, n) => {

        let rec loop = (list, i, acc) =>
            switch list {
            | list{} => list{acc->Prelude.List.reverse, list{}}
            | list{head, ...rest} => 
                if (i == 0) {
                    list{acc->Prelude.List.reverse, list}
                } else {
                    loop(rest, (i - 1), list{head, ...acc})
                }
            }   

        loop(list, n, list{})
        
    }
}

module Eighteen = {
    let slice : (list<'a>, int, int) => list<'a> =
        (list, start, end) => {

            let rec loop = (list, i, acc) => {
                switch list {
                    | list{} => acc
                    | list{head, ...rest} => 
                        switch i {
                            | i if (i < start) => loop(rest, (i+1), acc)
                            | i if (i >= start && i <= end) => 
                                loop(rest, (i + 1), list{head, ...acc})
                            | _ => acc
                        }

                }
            }

            switch (start, end) {
                | (start, end) if (start < 0 || end < 0) => list{}
                | _ => loop(list, 0, list{})->Prelude.List.reverse
            }
        }
}

module Nineteen = {
    let shiftItemsToTheLeft : (list<'a>, int)  => list<'a> =
        (list, count) => {

            let rec loop = (list, i, acc) => {
                switch list {
                    | list{} => acc->Prelude.List.reverse
                    | list{head, ...rest} => 
                        switch i {
                            | i if (i < count) => 
                                loop(rest, (i+1), list{head, ...acc})
                            | _ => 
                                list{head, ...rest}->Belt.List.concat(acc->Prelude.List.reverse)
                        }

                }
            }

            switch count {
                | count if (count > 0 && count < list->Prelude.List.length) => 
                    loop(list, 0, list{})
                | _ => list
            }
        }
}

module Twenty = {

    let insertInPlaceAlt : (list<'a>, ~pos: int, ~item: 'a) => list<'a> =
        (list, ~pos, ~item) => {

            let rec loop = (list, i, acc) => {
                switch list {
                    | list{} => acc
                    | list{head, ...rest} => 
                        if ( i == pos ) {
                            list{...acc, item}->Belt.List.concat(list{head, ...rest})
                        } else {
                            loop(rest, (i+1), list{...acc, head})
                        }
                }
            }

            switch pos {
                | pos if (pos >= 0) => 
                    if (pos < Prelude.List.length(list)) {
                        loop(list, 0, list{})
                    } else {
                        list{...list, item}
                    }
                | _ => list
            }
        }

    let rec insertInPlace : (list<'a>, ~pos: int, ~item: 'a) => list<'a> =
        (list, ~pos, ~item) => {
            
            switch list {
                | list if (pos < 0) => list
                | list if (pos > list->Prelude.List.length) => list{...list, item}
                | list{} => list{item}
                | list{head, ...rest} => 
                    if (pos == 0) {
                        list{item, head, ...rest}
                    } else {
                        list{
                            head, 
                            ...insertInPlace(
                                rest, 
                                ~pos= (pos - 1), 
                                ~item=item
                            )
                        }
                    }
            }
        }
}

module TwentyOne = {
    let rec listRange : (~start: int, ~end: int) => list<int> =
        (~start, ~end) => {
            
            switch start {
                | start if (start > end) => list{}
                | start if (start == end) => list{start}
                | _ => list{
                    start, 
                    ...listRange(
                        ~start=(start + 1),
                        ~end
                    )
                }
            }
        }
    
    // tail recursive version
    let listRangeAccum : (~start: int, ~end: int) => list<int> =
        (~start, ~end) => {
            
            let rec loop = (start, end, accum) => {
                switch start {
                    | start if (start > end) => list{}
                    | start if (start == end) => list{...accum, start}
                    | _ => loop((start+1), end, list{...accum, start})
                }
            }

            loop(start, end, list{})

        }
}

module TwentyTwo = {

    let rec lottery : (~sampleCount: int, ~stack: array<'a>) => array<'a> =
        (~sampleCount, ~stack) => {

            switch stack {
                | [] => []
                | [x] => [x]
                | _ => 
                    if (sampleCount > 0) {
                        let (winning_set, remaining) = Prelude.Array.pickRandom(stack)
                        winning_set
                            ->Js.Array.concat(
                                lottery(~sampleCount=(sampleCount - 1), ~stack=remaining)
                            )
                    } else {
                        []
                    }
            }
        }
}

module TwentyThree = {

    let rec shuffle : (array<'a>) => array<'a> = 
        (stack) => {

            switch stack {
                | [] => []
                | [x] => [x]
                | _ => 
                    let (winning_set, remaining) = Prelude.Array.pickRandom(stack)
                    winning_set->Js.Array.concat(shuffle(remaining))
            }
        }
}