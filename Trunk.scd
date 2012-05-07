
/*

Info: 

Trunk is an unfinished class that truncates SuperCollider code in order to make it shorter.
Typical SuperCollider tweets can be reduced by 50% in size, thus enabling more music to be written.

The encoder and decoder methods work most of the time, in particular the encoding one : )
The decoder method is there to tempt programmers to contribute an elegant solution. This one is not.

The class was made in one evening, I do not intend to work much more on it, but feel free to contribute.

GPL license (c) Thor Magnusson

*/

s.boot
t = Trunk.new

t.trans("SinOsc")
t.trans("S")

t.eval("{SinOsc.ar(333)+LFSaw.ar(2)*SinOsc.kr(22)}.play")

b = t.encode("play{LFSaw.ar(2)*SinOsc.ar(333)*SinOsc.kr(22)!2}")
b.interpret
c = t.decode(b)
c.interpret

b = "p{LP(Sw(333)*S.k(1.5),2e3*LT(2,a:1))}"
b.interpret
t.decode(b)

b = "p{LP(Sw(333)*S.k(1.5),2e3+LT(2,ip:1))}"
t.decode(b)

b = t.encode("{SinOsc.ar(3000)*Decay.ar(Dust.ar(22), 0.2)!2}.play")
b.interpret
t.decode(b)

b = t.encode("{(SinOsc.ar(333)+LFSaw.kr(2))*EnvGen.ar(Env.perc)*SinOsc.ar(33,p:0.2)}.play")
b.interpret
t.decode(b)


f{inf.l{p{EG(E.p)*S(1e3.r,m:0.1)!2};0.1.w}}


b = "f{inf.l{p{EG(E.p)*S(1e3.r,m:0.1)!2};0.1.w}}"
b.interpret
t.decode(b)


// some SuperCollider Tweets to test the lib on

t = Trunk.new

b = t.encode("{LocalOut.ar(a=CombN.ar(BPF.ar(LocalIn.ar(2)*7.5+Saw.ar([32,33],0.2),2**LFNoise0.kr(4/3,4)*300,0.1).distort,2,2,40));a}.play")
b.interpret
t.decode(b)

02 - LFSaw
b = t.encode("{Splay.ar(Ringz.ar(Impulse.ar([2, 1, 4], [0.1, 0.11, 0.12]), [0.1, 0.1, 0.5])) * EnvGen.kr(Env([1, 1, 0], [120, 10]), doneAction: 2)}.play")
b.interpret
t.decode(b)

03 - Tim Walters
b = t.encode("play{({|k|({|i|y=SinOsc;y.ar(i*k*k,y.ar(i*k**i/[4,5])*Decay.kr(Dust.kr(1/4**i),y.ar(0.1)+1*k+i,k*999))}!8).product}!16).sum}")
b.interpret 
t.decode(b)

04 - Nathaniel Virgo
x = a.encode("b=Buffer.read(s,'sounds/a11wlk01.wav');play{t=Impulse.kr(5);PlayBuf.ar(1,b,1,t,Demand.kr(t,0,Dseq(1e3*[103,41,162,15,141,52,124,190],4)))!2}")
x.interpret
a.decode(x)

05 - Batuhan Bozkurt
b = t.encode("play{f=LocalIn.ar(2).tanh;k=Latch.kr(f[0].abs,Impulse.kr(1/4));LocalOut.ar(f+CombC.ar(Blip.ar([4,6],100*k+50,0.9),1,k*0.3,50*f));f}")
b.interpret

07 - Thor Magnusson
b = t.encode("play{x=SinOsc;y=LFNoise0;a=y.ar(8);(x.ar(Pulse.ar(1)*24)+x.ar(90+(a*90))+MoogFF.ar(Saw.ar(y.ar(4,333,666)),a*XLine.ar(1,39,99,99,0,2)))!2/3}")
b.interpret

13 - Batuhan Bozkurt
b = t.encode("play{LeakDC.ar(BRF.ar(Saw.ar(8,Decay2.kr(x=Duty.kr(1/8,0,Drand([0,Drand((0.4,0.5..1))],inf)),0.01,0.3))**1.5,x*20+[45.1,45],0.1)).tanh}")
b.interpret

15 - Jason Dixon 
b = t.encode("{x=Array.fill(5,{[0.00001,0.03].asSpec.map(LFNoise2.kr(3))});Splay.ar(Friction.ar(LFTri.ar(50),friction:x,mass:x*30000))}.play")
b.interpret

16 - Batuhan Bozkurt
b = t.encode("play{AllpassC.ar(SinOsc.ar(55).tanh,0.4,TExpRand.ar(2e-4, 0.4,Impulse.ar(8)).round([2e-3,4e-3]),2)}")
b.interpret

17 - RedFrik
b = t.encode("{RHPF.ar(GbmanN.ar([2300,1150]),LFSaw.ar(Pulse.ar(4,[1,2]/8,1,LFPulse.ar(1/8)/5+1))+2)}.play")
b.interpret

18 - Nathaniel Virgo
b = t.encode("play{p=PinkNoise.ar(1!2);BRF.ar(p+Blip.ar(p+2,400),150,2,0.1)+LPF.ar(FreeVerb2.ar(*LPF.ar(p+0.2*Dust.ar(0.1),60)++[1,1,0.2,1e4]).tanh,2000)}")
b.interpret

19 - MCLD
b = t.encode("{a=[0.02,0.1,1,2,3,4]; k=LFPar.kr(a+0.5).sum; f=Latch.kr(k,Impulse.kr(a)); Splay.ar(SinOsc.ar(f*100+300)/5)}.play")
b.interpret

22 - MCLD
b = t.encode("play{a=Duty.kr(0.1,0,Dseq(fib(32).wrap(20,55).midicps,inf));HPF.ar(LeakDC.ar(Splay.ar(LFCub.ar([-1,a,a/3,-2])))*9).clip2/9}")
b.interpret


