signature BIGINT=  
sig
    type bigint
    exception someError  
    val add  : bigint * bigint -> bigint 
    val sub1 : bigint * bigint -> bigint 
    val mul: bigint * bigint -> bigint 
    val divi: bigint * bigint -> bigint 
    val find_dig: bigint * bigint* bigint -> bigint 
    val compare: bigint * bigint -> bool  
    val isequal: bigint * bigint -> bool  
    val normalize : bigint -> bigint 
    val modulus : bigint * bigint -> bigint 
    val gcd : bigint * bigint -> bigint 
    val mag : bigint-> bigint 
    val isneg : bigint -> bool 
    val negate: bigint-> bigint 
    val remove_zero: bigint-> bigint 
end ;

structure BigInt : BIGINT= 
struct 
    type bigint = string 
    exception someError  
(*  *)
(* we can convert to bigint using this *)
fun normalize(s)= 
        if size(s)=0 then "0"
        else s 
(*  *)
(* we can tell if negative or not *)
fun isneg(s) = 
    if size(s)=0 then false 
    else if substring(s,0,1)="~" then true 
    else false 
(*  *)
(* to tell the magnitude of a number *)
fun mag(s) = 
    if substring(s,0,1)="~" then substring(s,1,size(s)-1 )
    else s 
(*  *)
(* remove trailing zeros *)
fun remove_zero(s)=
    if s="0" then s
    else if substring(s,0,1)="~" then
        if  remove_zero(mag(s))="0" then "0" else "~"^remove_zero(mag(s))
    else if substring(s,0,1)="0" then remove_zero(substring(s,1,size(s)-1 ))
    else s ;
(*  *)
(* negative of a number *)
fun negate(s)= 
    if remove_zero(s) ="0" then "0"
    else if isneg(s) then remove_zero(mag(s) )
    else "~"^remove_zero(s) 
(*  *)
(* helper function for add *)
fun add1(a,b,carry) = 
    if (size(a)=0 orelse size(b)= 0) andalso carry=0  then a^b 
    else if (size(a)=0 orelse size(b)= 0) then let 
        val x = Int.toString(carry)
        in  
            add1(a,x,0)
        end 
    else let 
        val SOME a1 = Int.fromString(substring(a,size(a)-1,1))
        val SOME b1 = Int.fromString(substring(b,size(b)-1,1))
        val a2 = substring(a,0,size(a)-1)
        val b2 = substring(b,0,size(b)-1)
        val c = a1 + b1 + carry 
        val c1 = Int.toString(c mod 10 )
        val newcarry = c div 10 
    in 
        add1(a2,b2,newcarry)^c1 
    end; 
(*  *)
(* helper for sub1  *)
fun sub(s1, s2 , res,borrow )= 
    if size(s1)=1 andalso borrow=1 andalso size(s2)=1  then 
    let 
        val SOME a = Int.fromString(s1)
        val SOME b = Int.fromString(s2)
        val c = a-b-1
        val d = Int.toString(c)
    in 
        d^res 
    end 
    else if size(s2)=0 andalso borrow=0 then s1^res 
    else if size(s2)=0 andalso borrow=1 then sub(s1,"1",res,0)
    else 
    let 
        val SOME a = Int.fromString(substring(s1,size(s1)-1,1))
        val SOME b = Int.fromString(substring(s2,size(s2)-1,1))
        val c = a-b
        val c1 = c+10
        val c2= c-1 
        val c3= c2+10 
        val d = Int.toString(c)
        val d1 = Int.toString(c1)
        val d2 = Int.toString(c2)
        val d3 = Int.toString(c3)
        val s1new = substring(s1,0,size(s1)-1)
        val s2new = substring(s2,0,size(s2)-1)
    in 
        if borrow=0 then 
            if (c>=0) then sub(s1new,s2new,d^res,0)
            else sub(s1new,s2new,d1^res,1 )
        else 
            if (c2>=0) then sub(s1new,s2new,d2^res,0)
            else sub(s1new,s2new,d3^res,1 )
    end ;
(*  *)
(* true for s1>s2 *)
fun compare(s1,s2)= 
    if isneg(s1) andalso isneg(s2) then compare(mag(s1),mag(s2))
    else if remove_zero(s1)="0" andalso remove_zero(s2)="0" then false 
    else if isneg(s1) then false 
    else if isneg(s2) then true 
    else 
    let 
        val s1new=remove_zero(s1)
        val s2new=remove_zero(s2)
        val a = substring(s1new,0,1)
        val b = substring(s2new,0,1)
    in 
        if size(s1new)=1 andalso size(s2new)=1 andalso a=b then false  
        else 
        let 
            val c = substring(s1new,1,size(s1new)-1)
            val d = substring(s2new,1,size(s2new)-1)
        in 
            if size(s1new)=1 andalso size(s2new)=1 andalso a=b then false  
            else if size(s1new)>size(s2new) then true 
            else if size(s1new)<size(s2new) then false 
            else 
                if a>b then true
                else if a<b then false 
                else compare(c,d)
        end 
    end ;
(*  *)
fun isequal(a,b)= not(compare(a,b)) andalso not(compare(b,a))
(*  *)
(* helper for mul *)
fun reverse(s)=
    if size(s)=1 orelse size(s)=0 then s 
    else 
    let 
        val last = size(s) 
    in 
        substring(s,last-1,1)^reverse(substring(s,0,last-1))
    end;
(*  *)
fun mult2(s,num,carry)=
    if size(s)=0 
    then 
    let 
        val a= Int.toString(carry)
    in 
        a
    end 
    else 
    let 
        val a=substring(s,0,1)
        val SOME b= Int.fromString(a)
        val c= b*num + carry 
        val s_new =substring(s,1,size(s)-1)
        val new_carry = c div 10
        val d= c mod 10
        val e=Int.toString(d)
    in 
        e^mult2(s_new,num,new_carry)
    end;
(*  *)
fun mult(s,num)=
    let 
        val s_new = reverse(s)
        val ans_rev = mult2(s_new,num,0)
        val ans= reverse(ans_rev)
    in 
        ans
    end;
(*  *)
(* can add negatives *)
fun add(a, b) = 
    if isneg(a) andalso isneg(b) then "~"^add(remove_zero(mag(a)),remove_zero(mag(b)))
    else if isneg(a) then 
        let 
            val b1 = remove_zero(b)
            val a1 = remove_zero(mag(a))
            val d1 = compare(a1,b1) 
            val d2 = compare(b1,a1)
        in 
            if not(d1) andalso not(d2) then "0"
            else if d1 then "~"^sub(a1,b1,"",0)
            else sub(b1,a1,"",0)
        end 
    else if isneg(b) then add(b,a)
    else add1(remove_zero(a),remove_zero(b),0);
(*  *)
(* can subtract negatives *)
    fun sub1(a,b) = add(a,negate(b))
(*  *)
(* also for negatives *)
    fun mula(a,b)= 
    if size(b)=0 then "0"
    else if isneg(a) andalso isneg(b) then 
    let 
    val a1= substring(a,1,size(a)-1 )
    val b1 = substring(b,1,size(b)-1 )
    in 
        mula(a1,b1)
    end 
    else if isneg(a) then 
    let 
    val a1= substring(a,1,size(a)-1 )
    in  
        "~"^mula(a1,b)
    end 
    else if isneg(b) then 
    let 
    val b1= substring(b,1,size(b)-1 )
    in  
        "~"^mula(a,b1)
    end 
    else 
    let 
        val c1 = substring(b,size(b)-1,1)
        val SOME c1_prime= Int.fromString(c1) 
        val c2 = mult(a,c1_prime )
        val b1= substring(b,0 , size(b)-1 )
        val c3 = mula(a,b1)^ "0"
    in 
        add(c3, c2)
    end ; 
(*  *)
(* can multiply negatives *)
fun mul(a,b)= remove_zero(mula(a,b))
(*  *)
(* helper for divi  *)
fun find_dig(target, num ,a )= 
    if  not (compare( mul(num,a), target )) then a
    else find_dig(target , num , sub1(a,"1"))
(*  *)
(* i am proud to make this funtion *)
fun divi1(a,b,index,rem) = 
    if index+1=size(a) then find_dig(rem, remove_zero(b),"9")
    else find_dig(rem, remove_zero(b),"9")^divi1(a,remove_zero(b),index+1, sub1(rem, mul(b,find_dig(rem, remove_zero(b),"9")))^substring(a,index+1,1))
(*  *)
(* division for negatives same as positives, just that quotient is negative(when needed) *)
fun divi(a,b)= 
    if isneg(a) andalso isneg(b) then divi(mag(a), mag(b) )
    else if isneg(a) orelse isneg(b) then remove_zero("~"^divi(mag(a), mag(b)))
    else remove_zero(divi1(a,b,0,substring(a,0,1)))
(*  *)
(* only for postives *)
fun modulus(a,b)= 
    let 
        val c= divi(a,b) 
    in 
        sub1(a, mul(b,c) )
    end ;
(*  *)
(* only for positives *)
fun gcd1(a,b)= 
    let 
        val b1 = remove_zero(b) 
    in 
        if b1="0" then a 
        else 
            let 
                val c = modulus(a,b1) 
            in 
                gcd1(b1,c) 
            end 
    end 
(*  *)
(* i have returned gcd of negatives as a positive *)
fun gcd(a,b) = gcd1(mag(a) , mag(b))
(*  *)
end ; 



functor Rat(Bigint : BIGINT ):
sig  
(* in my implementation only num can be negative *)
    type rational 
    exception rat_error 
    val make_rat: BigInt.bigint * BigInt.bigint -> rational option
    val rat: BigInt.bigint -> rational option
    val reci: BigInt.bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply : rational * rational -> rational (* multiplication *)
    val divide : rational * rational -> rational option (* division *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
    val helpme : string-> rational
end =

struct
open BigInt
type rational = (BigInt.bigint*BigInt.bigint)
exception rat_error 

fun remove_zero(s)=
    if s="0" then s
    else if substring(s,0,1)="~" then
        if remove_zero(substring(s,1,size(s)-1))="0" then "0" else "~"^remove_zero(substring(s,1,size(s)-1))
    else if substring(s,0,1)="0" then remove_zero(substring(s,1,size(s)-1 ))
    else s ;
(*  *)
fun make_rat(a,b)= 
        if remove_zero(b)="0" then raise rat_error
        else 
        let 
            val g = BigInt.gcd(a,b) 
            val a1 = BigInt.divi(a,g)
            val b1 = BigInt.divi(b,g) 
        in 
            if BigInt.isneg(a1) andalso BigInt.isneg(b1) then SOME (BigInt.mag(a1), BigInt.mag(b1))
            else if BigInt.isneg(a1) then SOME (a1,b1)
            else if BigInt.isneg(b1) then SOME ("~"^a1, BigInt.mag(b1))
            else SOME (a1,b1) 
        end 
(*  *)
fun rat(a) = make_rat(a,BigInt.normalize("1"))
(*  *)
fun reci(a)= make_rat(BigInt.normalize("1"),a)
(*  *)
fun neg((b,c)) = (BigInt.negate(b),c)
(*  *)
fun inverse(a) = 
    let val (b,c) = a 
    in 
    if not (BigInt.mag(b)= "0") then 
        if not(BigInt.isneg(b)) then make_rat(c,b) 
        else make_rat("~"^c , mag(b) )
    else raise rat_error 
    end 
(*  *)
fun equal(a,b) = 
let 
    val (a1,a2)= a 
    val (b1,b2)= b 
    val x= BigInt.mul(a1,b2)
    val y = BigInt.mul(a2,b1)
    in 
    not(BigInt.compare(y,x)) andalso not(BigInt.compare(x,y)) 
    end 
(*  *)
fun less(a,b) = 
    let 
    val (a1,a2)= a 
    val (b1,b2)= b 
    val x= BigInt.mul(a1,b2)
    val y = BigInt.mul(a2,b1)
    in 
    BigInt.compare(y,x)  
    end 
(*  *) 
fun add(a,b)= 
    let 
    val (a1,a2)= a 
    val (b1,b2)= b 
    val x1= BigInt.mul(a1,b2)
    val y1 = BigInt.mul(a2,b1)
    val x = remove_zero(x1)
    val y= remove_zero(y1)
    in 
    valOf(make_rat((BigInt.add(y,x), remove_zero(BigInt.mul(a2,b2)))))
    end 
(*  *)
fun subtract(a,b) = add(a,neg(b))
(*  *)
fun multiply(a,b) = 
    let 
    val (a1,a2)= a 
    val (b1,b2)= b 
    val x= BigInt.mul(a1,b1)
    val y = BigInt.mul(a2,b2)
    in 
    valOf(make_rat(x,y) )
    end 
(*  *)
fun divide(a,b) = 
    let 
    val c = valOf(inverse(b))
    val c1 = multiply(a,c) 
    val c2 = SOME c1 
    in 
    c2 
    end 
(*  *)
fun showrat(a) = 
    let 
        val (b,c) = a 
        val SOME d = make_rat(b,c)
        val (e,f) = d 
    in 
        e^"/"^f
    end 
(*  *)
fun pos(s,ch,i)= 
    if i>= size(s) then size(s) 
    else if substring(s,i,1)=ch then i 
    else pos(s,ch,i+1)
(*  *)
fun fromDecimal(s) = 
    let 
        val dot = pos(s,".",0)
        val lbrack = pos(s,"(",0)
        val rbrack = pos(s,")",0)
    in 
        if dot = size(s) then (BigInt.normalize(s),BigInt.normalize("1"))
        else if lbrack = size(s) then 
        let 
            val left = substring(s,0,dot) 
            val right = substring(s,dot+1, size(s)-dot -1 )
            val num = String.concat(List.tabulate(size(right), fn _ => "0"))
            val num = "1"^num 
        in 
        valOf(make_rat(left^right, num ))
        end 
        else let 
            val left = substring(s,0,dot) 
            val mid = substring(s,dot+1,lbrack-dot-1  )
            val right= substring(s,lbrack+1 , rbrack-lbrack-1)
            val num1 = String.concat(List.tabulate(size(mid), fn _ => "0"))
            val num1 = "1"^num1
            val num2 = String.concat(List.tabulate(size(right)+size(mid), fn _ => "0"))
            val num2 = "1"^num2 
        in 
        valOf(make_rat(BigInt.sub1(left^mid^right, left^mid), BigInt.sub1(num2, num1)))
        end 
    end 
(*  *)
fun showRat(r) = 
    let 
        val (a,b) = r 
        val SOME r1 = make_rat(a,b) 
        val (x,y) = r1 
    in 
        x^"/"^y
    end 
(*  *)
fun nearest(n,num) = 
    if remove_zero(BigInt.modulus(num,n)) ="0" then num 
    else nearest(n, num^"9")
fun pow2 (n,num )= 
    if not(remove_zero(BigInt.modulus(n,"2")) ="0") then num 
    else pow2(BigInt.divi(n,"2"), BigInt.add(num,"1"))
fun pow5 (n,num )= 
    if not(remove_zero(BigInt.modulus(n,"5")) ="0") then num 
    else pow5(BigInt.divi(n,"5"), BigInt.add(num,"1"))
fun pow(x,n)=  
    if remove_zero(n) = "0" then "1"
    else BigInt.mul(pow(x,BigInt.sub1(n,"1")), x )
(*  *)
fun correction(s,n)= 
    if size(s) = n then s 
    else correction("0"^s, n )
(*  *)
fun showDecimal(r) = 
    let 
        val (a,b) = r 
    in 
        if not (BigInt.isneg(a)) then
        let 
            val x1 = pow2(b,"0")
            val SOME powerof2 = Int.fromString(x1)
            val y1 = pow5(b,"0")
            val SOME powerof5 = Int.fromString(y1)
            val left = size(BigInt.divi(a,b))
            val x2 = pow("2", x1 )
            val y2 = pow("5", y1 )
            val b_new =BigInt.divi(b,x2)
            val b_new1 =BigInt.divi(b_new,y2)
            val z1 = nearest(b_new1, "9")
            val right = size(z1)
            val ha = Int.max(powerof2, powerof5)
            val mul_factor= String.concat(List.tabulate(ha+right, fn _ => "0"))
            val mul_factor= "1"^mul_factor
            val mul_val = BigInt.mul(a,mul_factor)
            val raw = BigInt.divi(mul_val, b)
            val mid = size(raw)- left - right 
            val midshould = ha 
            
        in 
            if BigInt.compare(b,a) then 
            let 
                val left = 0 
                val mid = size(raw)- right
            in 
                (* waitnow(substring(raw,0,left)^"."^substring(raw, left, mid )^"("^substring(raw, size(raw)- right, right)^")", corrrection_factor)  *)
                "."^correction(substring(raw, left, mid ),midshould)^"("^substring(raw, size(raw)- right, right)^")"
            end 
            else 
                (* if not((size(raw)-mid-right) = 0 ) then  *)
                substring(raw,0,left)^"."^correction(substring(raw, left, mid ),midshould)^"("^substring(raw, size(raw)- right, right)^")"
                (* else  *)
                (* "."^substring(raw, size(raw)- mid - right, mid )^"("^substring(raw, size(raw)- right, right)^")" *)
        end 
    else 
    let 
        val a_new = substring(a,1,size(a)-1)
        val SOME r_new= make_rat(a_new, b)
    in 
        "~"^showDecimal(r_new )
    end 
    end 
(*  *)
fun toDecimal(r)= showDecimal(r) 
(*  *)
fun helpme(s) = 
    let val x= pos(s,"/", 0 )
    in valOf(make_rat(substring(s,0,x), substring(s,x+1,size(s)-x-1)))
    end 
end ; 


structure Rational = Rat(BigInt);

val x = valOf(Rational.make_rat("23357577","62"));
val y= valOf(Rational.make_rat("1","22445"));
val a= Rational.fromDecimal("3.14"); 
val a= Rational.helpme("~3/27"); 


(* structure BigInt  : BIGINT = BigInt 
open BigInt

val z = gcd("~5","14");
val z = mul("2","9");
val x = find_dig("5","2","9");
val x = divi("100","1"); *)

(* val x = divi("10","~3");
val x = mul("00004","4");
val x = mul("~004","8");
val x = mul("00013313413","~32"); *)




