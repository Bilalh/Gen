import Cocoa

func fact(n: Double) -> Double {
    var newValue : Double
    
    if (fmod(n, floor(n)) == 0) {
        newValue = round(exp(lgamma(n+1.0)))
    } else {
        newValue = exp(lgamma(n + 1))
    }
    
    return newValue
}

infix operator ^^ { precedence 160 }
func ^^ (radix: Double, power: Double) -> Double {
    return pow(radix, power)
}

// rehearsal

let q = 100.0
let dur = sqrt( 100*((100.0 - 1) ^^ 2) )
let pl = sqrt(  10000.0  )


let inner = q ^^ 2 + dur ^^ 2 + dur^^2 + pl^^2
let ans = sqrt(inner)
0.1 * ans


// bacp

let q_1 = 100.0
let q_2 = 100.0
let pre = sqrt(10000.0)
let cre=sqrt( 100*((100.0 - 1) ^^ 2) )

let insid=(  (q_1-q_2)^^2 * 7  + pre^^2 + cre^^2)
sqrt(insid)


// langford 

let l_1 = 10.0
let l_2 = 2.0

let n_1 = 50.0
let n_2 = 1.0

let inside = (  (l_2 - l_1)^^2  + (n_1 - n_2)^^2  )
sqrt(inside)


// scp

let scp_1 = 50.0
let scp_2 = 1.0

let s_inside = ( (scp_1 - scp_2) ^^ 2  * 3  )
sqrt(s_inside)



// need 90

// warehouse
let n_upper = 60.0
let n_stores = 60.0
let n_warehouses = 60.0

let capacity = sqrt(n_warehouses * (n_upper - 1)^^2)
let opencost = sqrt(n_warehouses * (n_upper - 1)^^2)
let cost     = sqrt(n_warehouses * n_stores * (n_upper - 1)^^2)


let w_inside =  (  (n_upper - 1)^^2 + (n_stores-1)^^2  + (n_warehouses)^^2
    +  (capacity - 0.0)^^2 + (opencost - 0.0)^^2 + (cost - 0.0)^^2 )
sqrt(w_inside)




