
// parse code
// send tweet
// decode

/*
s.boot
t = Trunk.new

t.trans("SinOsc")
t.trans("S")


t.eval("{SinOsc.ar(333)+LFSaw.ar(2)*SinOsc.kr(22)}.play")
b = t.encode("play{LFSaw.ar(2)+SinOsc.ar(333)*SinOsc.kr(22)}")
b.interpret
c = t.decode(b)
c.interpret

p{S(333)*S.k(2)}



b = t.encode("play{SinOsc.ar(333)+(LFSaw.ar(2)*Dust.ar(22))}")
b.interpret

b = t.encode("{SinOsc.ar(333)+LFSaw.kr(2)*Dust.ar(22)}.play")
b.interpret
t.decode(b)

b = t.eval("{SinOsc.ar(1000)*Saw.kr(12).range(1, 100*SinOsc.kr(0.14))+WhiteNoise.kr(0.1)}.play")

b
t.decode(b)

*/

Trunk {

	var dict;
	
	*new { 
		^super.new.initTweetLib;
	}

	initTweetLib {
	
		// the Twitter sclang
		
		dict = Dictionary.new;

// Oscillators
		dict[\SinOscFB] = "SFB";
		dict[\SinOsc] = "S";
		dict[\Saw] = "Sw";
		dict[\Pulse] = "P";
		dict[\Osc] = "O";
		dict[\Formant] = "F";
		dict[\LFSaw] = "LS";
		dict[\LFPar] = "LFP";
		dict[\LFCub] = "LC";
		dict[\LFTri] = "LT";
		dict[\VarSaw] = "VS";
		dict[\SyncSaw] = "SSw";
		dict[\Select] = "Sl";
		dict[\Impulse] = "I";
		dict[\Blip] = "Bl";

// Demand 
		dict[\Demand] = "D";
		dict[\Duty] = "Dy";
		dict[\TDuty] = "TDy";
		dict[\Dseries] = "Ds";
		dict[\Dgeom] = "Dg";
		dict[\Dseq] = "Dq";
		dict[\Dser] = "Dr";
		dict[\Dshuf] = "Df";
		dict[\Drand] = "Dd";
		dict[\Dxrand] = "Dxd";
		dict[\Dswitch] = "Dsw";
		dict[\Dwhite] = "Dw";
		dict[\Diwhite] = "Diw";
		dict[\Dbrown] = "Db";
		dict[\Dibrown] = "Dib";
		dict[\Dstutter] = "Dst";

// Noise

		dict[\Rand] = "R";
		dict[\TRand] = "TR";
		dict[\IRand] = "IR";
		dict[\WhiteNoise] = "N";
		dict[\BrownNoise] = "BN";
		dict[\PinkNoise] = "PN";
		dict[\ClipNoise] = "CN";
		dict[\GrayNoise] = "GN";
		dict[\Crackle] = "Crk";
		dict[\Logistic] = "Log";
		dict[\LFNoise0] = "LFN";
		dict[\LFNoise1] = "LFN1";
		dict[\LFNoise2] = "LFN2";
		dict[\LFClipNoise] = "LFCN";
		dict[\LFDNoise] = "LFDN";
		dict[\LFDNoise1] = "LFDN1";
		dict[\LFDNoise3] = "LFDN3";
		dict[\Dust] = "Dt";
		dict[\Dust2] = "Dt2";
		dict[\Hasher] = "Hr";

// Filters
		dict[\Resonz] = "Rz";
		dict[\OnePole] = "OP";
		dict[\OneZero] = "OZ";
		dict[\TwoPole] = "TP";
		dict[\TwoZero] = "TZ";
		dict[\Integrator] = "Int";
		dict[\Decay] = "Dc";
		dict[\Decay2] = "Dc2";
		dict[\Lag] = "Lg";
		dict[\Lag2] = "Lg2";
		dict[\Lag3] = "Lg3";
		dict[\Ramp] = "Rp";
		dict[\RLPF] = "RLP";
		dict[\RHPF] = "RHP";
		dict[\LPF] = "LP";
		dict[\HPF] = "HP";
		dict[\BPF] = "BP";
		dict[\BRF] = "RF";
		dict[\MidEQ] = "MEQ";
		dict[\FOS] = "FS";
		dict[\SOS] = "SS";
		dict[\Ringz] = "Rgz";
		dict[\MoogFF] = "MF";
		dict[\DetectSilence] = "DS";

		dict[\Splay] = "Sp";
		dict[\SplayZ] = "SpZ";
		dict[\SplayAz] = "SpAz";

		dict[\LocalIn] = "LI";
		dict[\LocalOut] = "LO";
		dict[\SoundIn] = "SI";

// Trig extensions
		dict[\Trig] = "T";
		dict[\Trig1] = "T1";
		dict[\PulseCount] = "PC";
		dict[\Stepper] = "St";
		dict[\PulseDivider] = "PD";
		dict[\ZeroCrossing] = "Z";
		dict[\Phasor] = "Ph";
		dict[\Latch] = "Lt";
		dict[\Gate] = "G";
		dict[\Peak] = "Pk";

// Reverb
		dict[\GVerb] = "GV";
		dict[\FreeVerb] = "FV";
		dict[\FreeVerb2] = "FV2";


// BUFIO extensiions
		dict[\PlayBuf] = "PB";
		dict[\TGrains] = "TG";
		dict[\BufRd] = "BR";
		dict[\BufWr] = "BW";
		dict[\RecordBuf] = "RB";
		dict[\LocalBuf] = "LB";

		dict[\Buffer] = "B";
		dict[\read] = "r";
		dict[\alloc] = "a";

// Envelope stuff

		dict[\EnvGen] = "EG";
		dict[\Linen] = "Ln";
		dict[\Env] = "E";
		dict[\triangle] = "t";
		dict[\sine] = "s";
		dict[\perc] = "p";
		dict[\linen] = "l";
		dict[\adsr] = "a";
		dict[\asr] = "ar";

// Lines
		dict[\Line] = "L";
		dict[\XLine] = "XL";
		dict[\LinExp] = "LE";
		dict[\LinLin] = "LL";

		dict[\ar] = "ZZZ"; // a trick due to complex encoding
		dict[\kr]= "k";

// function extensiions
		dict[\play] = "p";
		dict[\fork] = "f";
		dict[\scope] = "s";
		dict[\freqscope] = "fs";
		dict[\plot] = "pl";

// Object extensions

		dict[\postln] = "p";
		
		
// UGen extensions

		dict[\range] = "r";
		dict[\tanh] = "t";
		dict[\round] = "rd";
		dict[\distort] = "d";
		dict[\clip] = "c";
		dict[\clip2] = "c2";
		dict[\linlin] = "ll";
		dict[\linexp] = "le";
		dict[\explin] = "el";
		dict[\expexp] = "ee";
		
		dict[\doneAction] = "dA";
		dict[\add] = "a";
		dict[\mul] = "m";
		dict[\phase] = "p";
		dict[\iphase] = "ip";

// SimpleNumber

		dict[\wait] = "w";
		dict[\rand] = "r";
		dict[\rrand] = "rr";
		dict[\round] = "rd";
		dict[\loop] = "l";
		dict[\midicps] = "mc";
		dict[\cpsmidi] = "cm";
		dict[\distort] = "d";
		dict[\tanh] = "t";
		dict[\cubed] = "c";

// SequenceableCollection

		dict[\distort] = "d";
		dict[\tanh] = "t";
		dict[\rand] = "r";
		dict[\rrand] = "rr";
		dict[\cubed] = "c";
		dict[\squared] = "s";
		dict[\clip] = "cl";
		dict[\clip2] = "cl2";
		dict[\midicps] = "mc";

	}
	
	trans { arg class;
		^if(dict[class.asSymbol].isNil.not, {dict[class.asSymbol]}, {dict.findKeyForValueCS(class);});
	}
	
	eval {arg codestr;
		var encoded = this.encode(codestr);
		encoded.interpret;
		^encoded;
	}

	encode {arg codestr;
		// this algorithm takes into account UGens that are not part of TweetLib
		// it is overly complicated then, but ...
		var keyword = "";
		var newcode = "";
		var tempword = "";
		var lastfound = true;
		var rate;
		"-- Old char count : ".post; codestr.size.postln;
	
		codestr.do({arg char, i;
			if(char.isAlphaNum, { // making a word
				tempword=tempword++char;
				if(i==(codestr.size-1), {newcode = newcode ++ dict[tempword.asSymbol]; tempword=""});
			}, {
				if(tempword.size>0, {
					if(dict[tempword.asSymbol].isNil, { // if UGen does not exist
						rate = if(codestr[i..i+2] == ".ar", {".ZAR"}, {if(codestr[i..i+2] == ".kr", {".ZKR"}, {""})});
						keyword = tempword ++ rate;
						lastfound = false;
					},{
						if(lastfound, {keyword = dict[tempword.asSymbol]}, {keyword = tempword});
						lastfound = true;
					});
					newcode = newcode ++ keyword ++ char;
					tempword = "";
				},{					
					newcode = newcode ++ char;
				});
				if((char == $=) || (char == $() || (char == $,) || (char == $;)|| (char == $*)|| (char == $+)|| (char == $-)|| (char == $/) || (char == $.), {
					lastfound = true;
					if(dict[tempword.asSymbol].isNil.not, {keyword = dict[tempword.asSymbol]}, {keyword = tempword});					newcode = newcode ++ keyword;
					tempword = "";
				}); 
			});
		});
		newcode = newcode.replace(".ZZZ", "");
		newcode = newcode.replace(".XXX", ".k");
		newcode = newcode.replace(".ZKR", ".kr");
		newcode = newcode.replace(".ZAR", ".ar");
		newcode = newcode.replace(".kr.k", ".kr");
		newcode = newcode.replace(" ", "");
		if((newcode.last== $.), {newcode = newcode[0..newcode.size-2]; newcode = "p"++newcode});
		
		"-- New char count : ".post; newcode.size.postln;
		^newcode;
	}
	
	decode {arg codestr; // this is rubbish. who can make the best decoder?
		var newcode = "";
		var tempword = "";
		var rate = ".ar";
		var lasttype = \Function;
		var fFunc;
		fFunc = {arg tempword;
			var returnword;
			switch(lasttype)
			{\UGen} {		
				var dict = Dictionary.new;
				dict[\range] = "r";
				dict[\tanh] = "t";
				dict[\round] = "rd";
				dict[\distort] = "d";
				dict[\clip] = "c";
				dict[\clip2] = "c2";
				dict[\linlin] = "ll";
				dict[\linexp] = "le";
				dict[\explin] = "el";
				dict[\expexp] = "ee";				
				dict[\doneAction] = "dA";
				dict[\add] = "a";
				dict[\mul] = "m";
				dict[\phase] = "p";
				dict[\iphase] = "ip";
				returnword = dict.findKeyForValueCS(tempword);
			}
			{\Env} {		
				var dict = Dictionary.new;
				dict[\triangle] = "t";
				dict[\sine] = "s";
				dict[\perc] = "p";
				dict[\linen] = "l";
				dict[\adsr] = "a";
				dict[\asr] = "ar";
				returnword = dict.findKeyForValueCS(tempword);
			}
			{\SimpleNumber} {		
				var dict = Dictionary.new;
				dict[\wait] = "w";
				dict[\rand] = "r";
				dict[\rrand] = "rr";
				dict[\round] = "rd";
				dict[\loop] = "l";
				dict[\midicps] = "mc";
				dict[\cpsmidi] = "cm";
				dict[\distort] = "d";
				dict[\tanh] = "t";
				dict[\cubed] = "c";
				returnword = dict.findKeyForValueCS(tempword);
			}
			{\SequenceableCollection} {		
				var dict = Dictionary.new;
				dict[\distort] = "d";
				dict[\tanh] = "t";
				dict[\rand] = "r";
				dict[\rrand] = "rr";
				dict[\cubed] = "c";
				dict[\squared] = "s";
				returnword = dict.findKeyForValueCS(tempword);
			}
			{\Function} {		
				var dict = Dictionary.new;
				dict[\play] = "p";
				dict[\fork] = "f";
				dict[\scope] = "s";
				dict[\freqscope] = "fs";
				dict[\plot] = "pl";
				returnword = dict.findKeyForValueCS(tempword);
			};
			// if it doesn't exist in the local dicts above, go to the main one
			returnword = returnword ? dict.findKeyForValueCS(tempword);
			returnword;
		};		
		
		codestr.do({arg char, i; 
			if(char.isAlphaNum, {
				tempword=tempword++char;
			}, {
				if(tempword.size==0, {
					newcode = newcode ++ char;
				}, {
					switch(true)
					{tempword.interpret.respondsTo(\ar) || (codestr[i] == $) )} { // It's a UGen
						rate = switch(codestr[i])
						{$.} {".kr"}
						{$(} {".ar"}
						{""};
						if(dict.findKeyForValueCS(tempword).isNil.not, {
							newcode = newcode ++ fFunc.value(tempword) ++ rate ++ char;
						}, {
							newcode = newcode ++ tempword ++ char;
						});
						if(tempword == "E", {
							lasttype = \Env;
						}, {
							lasttype = \UGen;
						});
					}
					{try{tempword.interpret}.isKindOf(SimpleNumber)} {  // it's a Number
						if(fFunc.value(tempword).isNil, {
							newcode = newcode ++ tempword ++ char;
						},{
							newcode = newcode ++ fFunc.value(tempword) ++ char;
						});
						lasttype = \SimpleNumber;
					}
					{try{tempword.interpret}.isKindOf(SequenceableCollection)} {  // it's an Array 
						if(fFunc.value(tempword).isNil, {
							newcode = newcode ++ tempword ++ char;
						},{
							newcode = newcode ++ fFunc.value(tempword) ++ char;
						});
						lasttype = \SequenceableCollection;
					}
					{(codestr[i] == ${ ) || (codestr[i] == $} )} {  // it's a Function
						if(fFunc.value(tempword).isNil, {
							newcode = newcode ++ tempword ++ char;
						},{
							newcode = newcode ++ fFunc.value(tempword) ++ char;
						});
						lasttype = \Function;
					}
					{char == $= } { // it's a Variable
						newcode = newcode ++ tempword ++ char;
						lasttype = \Variable;
					}
					{ // it's everything else
						if(codestr[i] == $:, {
							lasttype = \UGen;
							newcode = newcode ++ fFunc.value(tempword) ++ char;
						}, {
							if(fFunc.value(tempword).isNil, {
								newcode = newcode ++ tempword ++ char;
							},{
								newcode = newcode ++ fFunc.value(tempword) ++ char;
							});
							lasttype = nil;
						});
						
					};
					tempword = "";
				})
			}) 
		});
		// getting very lazy here - this is crazy anyway
		newcode = newcode.replace(".kr.kr", ".kr");
		newcode = newcode.replace(".kr.k", ".kr");
		newcode = newcode.replace("Env.ar", "Env");
		newcode = newcode.replace("}.", "}.play");
		newcode = newcode.replace("}.nil", "}.play");
		newcode = newcode.replace("Env.kr", "Env");
		^newcode;
	}
}

O { // Osc
	*new { arg bufnum, freq=440.0, p=0.0, m=1.0, a=0.0;
		^Osc.ar(bufnum, freq, p, m, a)
	}
	*k { arg bufnum, freq=440.0, p=0.0, m=1.0, a=0.0;
		^Osc.kr(bufnum, freq, p, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

S { // SinOsc
	*new { arg freq=440.0, p=0.0, m=1.0, a=0.0;
		^SinOsc.ar(freq, p, m, a)
	}
	*k { arg freq=440.0, p=0.0, m=1.0, a=0.0;
		^SinOsc.kr(freq, p, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

SFB { // SinOscFB
	*new { arg freq=440.0, feedback=0.0, m=1.0, a=0.0;
		^SinOscFB.ar(freq, feedback, m, a)
	}
	*k { arg freq=440.0, feedback=0.0, m=1.0, a=0.0;
		^SinOscFB.kr(freq, feedback, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }	
}

F { // Formant
	*new { arg fundfreq = 440.0, formfreq = 1760.0, bwfreq = 880.0, m = 1.0, a = 0.0;
		^Formant.ar(fundfreq, formfreq, bwfreq, m, a)
	}
	*ar { ^this.new }
}

LS { // LFSaw
	*new { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFSaw.ar(freq, ip, m, a)
	}
	*k { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFSaw.kr(freq, ip, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFP { // LFPar
	*new { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFPar.ar(freq, ip, m, a)
	}
	*k { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFPar.kr(freq, ip, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LC { // LFCub
	*new { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFCub.ar(freq, ip, m, a)
	}
	*k { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFCub.kr(freq, ip, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}


LT { // LFTri
	*new { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFTri.ar(freq, ip, m, a)
	}
	*k { arg freq = 440.0, ip = 0.0, m = 1.0, a = 0.0;
		^LFTri.kr(freq, ip, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

VS { // VarSaw
	*new { arg freq = 440.0, ip = 0.0, width = 0.5, m = 1.0, a = 0.0;
		^VarSaw.ar(freq, ip, width, m, a)
	}
	*k { arg freq = 440.0, ip = 0.0, width = 0.5, m = 1.0, a = 0.0;
		^VarSaw.kr(freq, ip, width, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

SSw { // SyncSaw
	*new { arg syncFreq = 440.0, sawFreq = 440.0, m = 1.0, a = 0.0;
		^SyncSaw.ar(syncFreq, sawFreq, m, a)
	}
	*k { arg syncFreq = 440.0, sawFreq = 440.0, m = 1.0, a = 0.0;
		^SyncSaw.kr(syncFreq, sawFreq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Sl { // Select
	*new { arg which, array;
		^Select.ar(which, array)
	}
	*k { arg which, array;
		^Select.kr(which, array)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

I { // Impulse
	*new { arg freq = 440.0, p = 0.0, m = 1.0, a = 0.0;
		^Impulse.ar(freq, p, m, a)
	}
	*k { arg freq = 440.0, p = 0.0, m = 1.0, a = 0.0;
		^Impulse.kr(freq, p, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Bl { // Blip
	*new { arg freq=440.0, numharm = 200.0, m = 1.0, a = 0.0;
		^Blip.ar(freq, numharm, m, a)
	}
	*k { arg freq=440.0, numharm = 200.0, m = 1.0, a = 0.0;
		^Blip.kr(freq, numharm, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Sw { // Saw
	*new { arg freq=440.0, m = 1.0, a = 0.0;
		^Saw.ar(freq, m, a)
	}
	*k { arg freq=440.0, m = 1.0, a = 0.0;
		^Saw.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

P { // Pulse
	*new { arg freq=440.0, width = 0.5, m = 1.0, a = 0.0;
		^Pulse.ar(freq, width, m, a)
	}
	*k { arg freq=440.0, width = 0.5, m = 1.0, a = 0.0;
		^Pulse.kr(freq, width, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

D { // Demand
	*new { arg trig, reset, demandUGens;
		^Demand.ar(trig, reset, demandUGens)
	}
	*k { arg trig, reset, demandUGens;
		^Demand.kr(trig, reset, demandUGens)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Dy { // Duty
	*new { arg dur = 1.0, reset = 0.0, level = 1.0, dA = 0;
		^Duty.ar(dur, reset, level, dA)
	}
	*k { arg dur = 1.0, reset = 0.0, level = 1.0, dA = 0;
		^Duty.kr(dur, reset, level, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

TDy { // TDuty
	*new { arg dur = 1.0, reset = 0.0, level = 1.0, dA = 0, gapFirst = 0;
		^TDuty.ar(dur, reset, level, dA, gapFirst)
	}
	*k { arg dur = 1.0, reset = 0.0, level = 1.0, dA = 0, gapFirst = 0;
		^TDuty.kr(dur, reset, level, dA, gapFirst)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Ds { // Dseries
	*new { arg start = 1, step = 1, length = inf;
		^Dseries(length, start, length)
	}
}

Dg { // Dgeom
	*new { arg start = 1, grow = 2, length = inf;
		^Dgeom(length, grow, length)
	}
}

Dq { // Dseq
	*new { arg list, repeats = 1;
		^Dseq(list, repeats)
	}
}

Dr { // Dser
	*new { arg list, repeats = 1;
		^Dser(list, repeats)
	}
}

Df { // Dshuf
	*new { arg list, repeats = 1;
		^Dshuf(list, repeats)
	}
}

Dd { // Drand
	*new { arg list, repeats = 1;
		^Drand(list, repeats)
	}
}

Dxd { // Dxrand
	*new { arg list, repeats = 1;
		^Dxrand(list, repeats)
	}
}

Dsw { // Dswitch
	*new { arg list, index;
		^Dswitch(list, index)
	}	
}

Dw { // Dwhite
	*new { arg lo = 0.0, hi = 1.0, length = inf;
		^Dwhite(length, lo, hi)
	}
}

Diw { // Diwhite
	*new { arg lo = 0.0, hi = 1.0, length = inf;
		^Diwhite(length, lo, hi)
	}
}

Db { // Dbrown
	*new { arg lo = 0.0, hi = 1.0, length = inf;
		^Dbrown(length, lo, hi)
	}
}

Dib { // Dibrown
	*new { arg lo = 0.0, hi = 1.0, length = inf;
		^Dibrown(length, lo, hi)
	}
}

Dst { // Dibrown
	*new { arg n, in;
		^Dstutter(n, in)
	}
}

// noise
R { // Rand
	*new { arg lo = 0.0, hi = 1.0;
		^Rand.ar(lo, hi)
	}
	*ar { ^this.new }
}

IR { // IRand
	*new { arg lo = 0.0, hi = 1.0;
		^IRand.ar(lo, hi)
	}
	*ar { ^this.new }
}

TR { // TRand
	*new { arg lo = 0.0, hi = 1.0, trig = 0.0;
		^TRand.ar(lo, hi, trig)
	}
	*k { arg lo = 0.0, hi = 1.0, trig = 0.0;
		^TRand.kr(lo, hi, trig)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

N { // WhiteNoise
	*new { arg m = 1.0, a = 0.0;
		^WhiteNoise.ar(m, a)
	}
	*k { arg m = 1.0, a = 0.0;
		^WhiteNoise.kr(m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }	
}

BN { // BrownNoise
	*new { arg m = 1.0, a = 0.0;
		^BrownNoise.ar(m, a)
	}
	*k { arg m = 1.0, a = 0.0;
		^BrownNoise.kr(m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

PN { // PinkNoise
	*new { arg m = 1.0, a = 0.0;
		^PinkNoise.ar(m, a)
	}
	*k { arg m = 1.0, a = 0.0;
		^PinkNoise.kr(m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

CN { // ClipNoise
	*new { arg m = 1.0, a = 0.0;
		^ClipNoise.ar(m, a)
	}
	*k { arg m = 1.0, a = 0.0;
		^ClipNoise.kr(m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

GN { // GrayNoise
	*new { arg m = 1.0, a = 0.0;
		^GrayNoise.ar(m, a)
	}
	*k { arg m = 1.0, a = 0.0;
		^GrayNoise.kr(m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Crk { // Crackle
	*new { arg chaosParam=1.5, m = 1.0, a = 0.0;
		^Crackle.ar(chaosParam, m, a)
	}
	*k { arg chaosParam=1.5, m = 1.0, a = 0.0;
		^Crackle.kr(chaosParam, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Log { // Logistic
	*new { arg chaosParam=3.0, freq = 1000.0, init= 0.5, m = 1.0, a = 0.0;
		^Logistic.ar(chaosParam, freq, init, m, a)
	}
	*k { arg chaosParam=3.0, freq = 1000.0, init= 0.5, m = 1.0, a = 0.0;
		^Logistic.kr(chaosParam, freq, init, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFN { // LFNoise0
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise0.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise0.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFN1 { // LFNoise1
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise1.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise1.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFN2 { // LFNoise2
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise2.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFNoise2.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFCN { // LFClipNoise
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFClipNoise.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFClipNoise.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFDN { // LFDNoise0
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise0.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise0.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFDN1 { // LFDNoise1
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise1.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise1.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LFDN3 { // LFDNoise3
	*new { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise3.ar(freq, m, a)
	}
	*k { arg freq=500.0, m = 1.0, a = 0.0;
		^LFDNoise3.kr(freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Dt { // Dust
	*new { arg density = 0.0, m = 1.0, a = 0.0;
		^Dust.ar(density, m, a)
	}
	*k { arg density = 0.0, m = 1.0, a = 0.0;
		^Dust.kr(density, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Dt2 { // Dust2
	*new { arg density = 0.0, m = 1.0, a = 0.0;
		^Dust2.ar(density, m, a)
	}
	*k { arg density = 0.0, m = 1.0, a = 0.0;
		^Dust2.kr(density, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Hr { // Dust2
	*new { arg in = 0.0, m = 1.0, a = 0.0;
		^Hasher.ar(in, m, a)
	}
	*k { arg in = 0.0, m = 1.0, a = 0.0;
		^Hasher.kr(in, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

PB { // PlayBuf
	*new { arg numChannels, bufnum=0, rate=1.0, trigger=1.0, startPos=0.0, loop = 0.0, dA=0;
		^PlayBuf.ar(numChannels, bufnum, rate, trigger, startPos, loop, dA)
	}
	*k { arg numChannels, bufnum=0, rate=1.0, trigger=1.0, startPos=0.0, loop = 0.0, dA=0;
		^PlayBuf.kr(numChannels, bufnum, rate, trigger, startPos, loop, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

TG { // TGrains
	*new { arg  numChannels, trigger=0, bufnum=0, rate=1, centerPos=0, dur=0.1, pan=0, amp=0.1, interp=4;
		^TGrains.ar(numChannels, trigger, bufnum, rate, centerPos, dur, pan, amp, interp)
	}
	*ar { ^this.new }
}

BR { // BufRd
	*new { arg numChannels, bufnum=0, p=0.0, loop=1.0, interpolation=2;
		^BufRd.ar(numChannels, bufnum, p, loop, interpolation)
	}
	*k { arg numChannels, bufnum=0, p=0.0, loop=1.0, interpolation=2;
		^BufRd.kr(numChannels, bufnum, p, loop, interpolation)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

BW { // BufWr
	*new { arg inputArray, bufnum=0, p=0.0, loop=1.0;
		^BufWr.ar(inputArray, bufnum, p, loop)
	}
	*k { arg inputArray, bufnum=0, p=0.0, loop=1.0;
		^BufWr.kr(inputArray, bufnum, p, loop)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

RB { // RecordBuf
	*new { arg inputArray, bufnum=0, offset=0.0, recLevel=1.0, preLevel=0.0, run=1.0, loop=1.0, trigger=1.0, dA=0;
		^RecordBuf.ar(inputArray, bufnum, offset, recLevel, preLevel, run, loop, trigger, dA)
	}
	*k { arg inputArray, bufnum=0, offset=0.0, recLevel=1.0, preLevel=0.0, run=1.0, loop=1.0, trigger=1.0, dA=0;
		^RecordBuf.kr(inputArray, bufnum, offset, recLevel, preLevel, run, loop, trigger, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LB { // LocalBuf
	*new { arg numFrames = 1, numChannels = 1;
		^LocalBuf.new(numFrames, numChannels)
	}
}

B { // Buffer
	*new { arg server, numFrames, numChannels, bufnum;
		^Buffer.new(server, numFrames, numChannels, bufnum)
	}
	*a { arg server, numFrames, numChannels = 1, completionMessage, bufnum;
		^Buffer.alloc(server, numFrames, numChannels, completionMessage, bufnum)
	}
	*r { arg server,path,startFrame = 0,numFrames = -1, action, bufnum;
		^Buffer.read(server,path,startFrame,numFrames, action, bufnum)
	}
}

// Trig 
T { // Trig
	*new { arg in = 0.0, dur = 0.1;
		^Trig.ar(in, dur)
	}
	*k { arg in = 0.0, dur = 0.1;
		^Trig.kr(in, dur)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

T1 { // Trig1
	*new { arg in = 0.0, dur = 0.1;
		^Trig1.ar(in, dur)
	}
	*k { arg in = 0.0, dur = 0.1;
		^Trig1.kr(in, dur)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Lt { // Latch
	*new { arg in = 0.0, trig = 0.1;
		^Latch.ar(in, trig)
	}
	*k { arg in = 0.0, trig = 0.1;
		^Latch.kr(in, trig)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

G { // Gate
	*new { arg in = 0.0, trig = 0.1;
		^Gate.ar(in, trig)
	}
	*k { arg in = 0.0, trig = 0.1;
		^Gate.kr(in, trig)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

PC { // PulseCount
	*new { arg trig = 0.0, reset = 0.0;
		^PulseCount.ar(trig, reset)
	}
	*k { arg trig = 0.0, reset = 0.0;
		^PulseCount.kr(trig, reset)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Pk { // Peak
	*new { arg trig = 0.0, reset = 0.0;
		^Peak.ar(trig, reset)
	}
	*k { arg trig = 0.0, reset = 0.0;
		^Peak.kr(trig, reset)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

GV { // GVerb
	*new { arg in, roomsize = 10, revtime = 3, damping = 0.5, inputbw =  0.5, spread = 15,
			drylevel = 1, earlyreflevel = 0.7, taillevel = 0.5, maxroomsize = 300, m = 1, a = 0;
		^GVerb.ar(in, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize, m, a)
	}
	*ar { ^this.new }
}

FV { // FreeVerb
	*new { arg in, mix = 0.33, room = 0.5, damp = 0.5, m = 1.0, a = 0.0;
		^FreeVerb.ar(in, mix, room, damp, m, a)
	}
	*ar { ^this.new }
}

FV2 { // FreeVerb
	*new { arg in,in2, mix = 0.33, room = 0.5, damp = 0.5, m = 1.0, a = 0.0;
		^FreeVerb2.ar(in, in2, mix, room, damp, m, a)
	}
	*ar { ^this.new }
}

St { // Stepper
	*new { arg trig=0, reset=0, min=0, max=7, step=1, resetval;
		^Stepper.ar(trig, reset, min, max, step, resetval)
	}
	*k { arg trig=0, reset=0, min=0, max=7, step=1, resetval;
		^Stepper.kr(trig, reset, min, max, step, resetval)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Pd { // PulseDivider
	*new { arg trig = 0.0, div = 2.0, start = 0.0;
		^PulseDivider.ar(trig, div, start)
	}
	*k { arg trig = 0.0, div = 2.0, start = 0.0;
		^PulseDivider.kr(trig, div, start)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Z { // ZeroCrossing
	*new { arg in=0.0;
		^ZeroCrossing.ar(in)
	}
	*k { arg in=0.0;
		^ZeroCrossing.kr(in)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Ph { // Phasor
	*new { arg trig = 0.0, rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0;
		^Phasor.ar(trig, rate, start, end, resetPos)
	}
	*k { arg trig = 0.0, rate = 1.0, start = 0.0, end = 1.0, resetPos = 0.0;
		^Phasor.kr(trig, rate, start, end, resetPos)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}


EG { // EnvGen - // note doneAction is renamed "dA" and it's default value is 2 (free synth)
	*new { arg envelope, gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, dA = 2;
		^EnvGen.ar(envelope, gate, levelScale, levelBias, timeScale, dA)
	}
	*k { arg envelope, gate = 1.0, levelScale = 1.0, levelBias = 0.0, timeScale = 1.0, dA = 2;
		^EnvGen.kr(envelope, gate, levelScale, levelBias, timeScale, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Ln { // only kr, but I implement ar, incase encoding and decoding do that
	*new { arg gate = 1.0, attackTime = 0.01, susLevel = 1.0, releaseTime = 1.0, dA = 0;
		^Linen.kr(gate, attackTime, susLevel, releaseTime, dA)
	}
	*k  { ^this.new }
	*ar { ^this.new }
	*kr { ^this.new }
}

E { // Env
	*new { arg levels=#[0,1,0], times=#[1,1], curve='lin', releaseNode, loopNode;
		^Env.new(levels, times, curve, releaseNode, loopNode)
	}
	*t { arg dur=1.0, level=1.0;
		^Env.triangle(dur, level)
	}
	*s { arg dur=1.0, level=1.0;
		^Env.sine(dur, level)
	}
	*p { arg attackTime=0.01, releaseTime=1.0, level=1.0, curve = -4.0;
		^Env.perc(attackTime, releaseTime, level, curve)
	}
	*l { arg attackTime=0.01, sustainTime=1.0, releaseTime=1.0, level=1.0, curve = \lin;
		^Env.linen(attackTime, sustainTime, releaseTime, level, curve)
	}
	*a { arg attackTime=0.01, decayTime=0.3, sustainLevel=0.5, releaseTime=1.0, peakLevel=1.0, curve = -4.0, bias = 0.0;
		^Env.adsr(attackTime, decayTime, sustainLevel, releaseTime, peakLevel, curve, bias)
	}
	*ar { arg attackTime=0.01, sustainLevel=1.0, releaseTime=1.0, curve = -4.0;
		^Env.asr(attackTime, sustainLevel, releaseTime, curve)
	}
}

L { // Line - // note doneAction is renamed "dA" and it's default value is 2 (free synth)
	*new { arg start=0.0, end = 1.0, dur = 1.0, m = 1.0, a = 0.0, dA = 2;
		^Line.ar(start, end, dur, m, a, dA)
	}
	*k { arg start=0.0, end = 1.0, dur = 1.0, m = 1.0, a = 0.0, dA = 2;
		^Line.kr(start, end, dur, m, a, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}


LE { // LinExp - 
	*new { arg in=0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
		^LinExp.ar(in, srclo, srchi, dstlo, dsthi)
	}
	*k { arg in=0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
		^LinExp.kr(in, srclo, srchi, dstlo, dsthi)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LL { // LinLin - // 
	*new { arg in=0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
		^LinLin.ar(in, srclo, srchi, dstlo, dsthi)
	}
	*k { arg in=0.0, srclo = 0.0, srchi = 1.0, dstlo = 1.0, dsthi = 2.0;
		^LinLin.kr(in, srclo, srchi, dstlo, dsthi)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

XL { // Line - // note doneAction is renamed "dA" and it's default value is 2 (free synth)
	*new { arg start=0.0, end = 1.0, dur = 1.0, m = 1.0, a = 0.0, dA = 2;
		^XLine.ar(start, end, dur, m, a, dA)
	}
	*k { arg start=0.0, end = 1.0, dur = 1.0, m = 1.0, a = 0.0, dA = 2;
		^XLine.kr(start, end, dur, m, a, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Rz { // Resonz
	*new { arg in = 0.0, freq = 440.0, bwr = 1.0, m = 1.0, a = 0.0;
		^Resonz.ar(in, freq, bwr, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, bwr = 1.0, m = 1.0, a = 0.0;
		^Resonz.kr(in, freq, bwr, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

OP { // OnePole
	*new { arg in = 0.0, coef = 0.5, m = 1.0, a = 0.0;
		^OnePole.ar(in, coef, m, a)
	}
	*k { arg in = 0.0, coef = 0.5, m = 1.0, a = 0.0;
		^OnePole.kr(in, coef, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

OZ { // OneZero
	*new { arg in = 0.0, coef = 0.5, m = 1.0, a = 0.0;
		^OneZero.ar(in, coef, m, a)
	}
	*k { arg in = 0.0, coef = 0.5, m = 1.0, a = 0.0;
		^OneZero.kr(in, coef, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

TP { // TwoPole
	*new { arg in = 0.0, freq = 440.0, radius = 0.8, m = 1.0, a = 0.0;
		^TwoPole.ar(in, freq, radius, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, radius = 0.8, m = 1.0, a = 0.0;
		^TwoPole.kr(in, freq, radius, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

TZ { // TwoZero
	*new { arg in = 0.0, freq = 440.0, radius = 0.8, m = 1.0, a = 0.0;
		^TwoZero.ar(in, freq, radius, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, radius = 0.8, m = 1.0, a = 0.0;
		^TwoZero.kr(in, freq, radius, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Int { // Integrator
	*new { arg in = 0.0, coef = 1.0, m = 1.0, a = 0.0;
		^Integrator.ar(in, coef, m, a)
	}
	*k { arg in = 0.0, coef = 1.0, m = 1.0, a = 0.0;
		^Integrator.kr(in, coef, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Dc { // Decay
	*new { arg in = 0.0, decayTime = 1.0, m = 1.0, a = 0.0;
		^Decay.ar(in, decayTime, m, a)
	}
	*k { arg in = 0.0, decayTime = 1.0, m = 1.0, a = 0.0;
		^Decay.kr(in, decayTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Dc2 { // Decay2
	*new { arg in = 0.0, attackTime = 0.01, decayTime = 1.0, m = 1.0, a = 0.0;
		^Decay2.ar(in, attackTime, decayTime, m, a)
	}
	*k { arg in = 0.0, attackTime = 0.01, decayTime = 1.0, m = 1.0, a = 0.0;
		^Decay2.kr(in, attackTime, decayTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Lg { // Lag
	*new { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag.ar(in, lagTime, m, a)
	}
	*k { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag.k(in, lagTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Lg2 { // Lag2
	*new { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag2.ar(in, lagTime, m, a)
	}
	*k { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag2.k(in, lagTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Lg3 { // Lag
	*new { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag3.ar(in, lagTime, m, a)
	}
	*k { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Lag3.k(in, lagTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}


Rp { // Ramp
	*new { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Ramp.ar(in, lagTime, m, a)
	}
	*k { arg in = 0.0, lagTime = 0.1, m = 1.0, a = 0.0;
		^Ramp.k(in, lagTime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

RLP { // RLPF
	*new { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^RLPF.ar(in, freq, rq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^RLPF.kr(in, freq, rq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

RHP { // RHPF
	*new { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^RHPF.ar(in, freq, rq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^RHPF.kr(in, freq, rq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LP { // LPF
	*new { arg in = 0.0, freq = 440.0, m = 1.0, a = 0.0;
		^LPF.ar(in, freq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, m = 1.0, a = 0.0;
		^LPF.kr(in, freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

HP { // HPF
	*new { arg in = 0.0, freq = 440.0, m = 1.0, a = 0.0;
		^HPF.ar(in, freq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, m = 1.0, a = 0.0;
		^HPF.kr(in, freq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

BP { // BPF
	*new { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^BPF.ar(in, freq, rq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^BPF.kr(in, freq, rq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

RF { // BRF
	*new { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^BRF.ar(in, freq, rq, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, rq = 1.0, m = 1.0, a = 0.0;
		^BRF.kr(in, freq, rq, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

MEQ { // MidEQ
	*new { arg in = 0.0, freq = 440.0, rq = 1.0, db = 0.0, m = 1.0, a = 0.0;
		^MidEQ.ar(in, freq, rq, db, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, rq = 1.0, db = 0.0, m = 1.0, a = 0.0;
		^MidEQ.kr(in, freq, rq, db, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

FS { // FOS
	*new { arg in = 0.0, a0 = 0.0, a1 = 0.0, b1 = 0.0, m = 1.0, a = 0.0;
		^FOS.ar(in, a0, a1, b1, m, a)
	}
	*k { arg in = 0.0, a0 = 0.0, a1 = 0.0, b1 = 0.0, m = 1.0, a = 0.0;
		^FOS.kr(in, a0, a1, b1, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

SS { // SOS
	*new { arg in = 0.0, a0 = 0.0, a1 = 0.0, a2 = 0.0, b1 = 0.0, b2 = 0.0, m = 1.0, a = 0.0;
		^SOS.ar(in, a0, a1, a2, b1, b2, m, a)
	}
	*k { arg in = 0.0, a0 = 0.0, a1 = 0.0, a2 = 0.0, b1 = 0.0, b2 = 0.0, m = 1.0, a = 0.0;
		^SOS.kr(in, a0, a1, a2, b1, b2, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Rgz { // Ringz
	*new { arg in = 0.0, freq = 440.0, decaytime = 1.0, m = 1.0, a = 0.0;
		^Ringz.ar(in, freq, decaytime, m, a)
	}
	*k { arg in = 0.0, freq = 440.0, decaytime = 1.0, m = 1.0, a = 0.0;
		^Ringz.kr(in, freq, decaytime, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}


MF { // MoogFF
	*new { arg in, freq=100, gain=2, reset=0, m=1, a=0;
		^MoogFF.ar(in, freq, gain, reset, m, a)
	}
	*k { arg in, freq=100, gain=2, reset=0, m=1, a=0;
		^MoogFF.kr(in, freq, gain, reset, m, a)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

DS { // DetectSilence
	*new { arg in = 0.0, amp = 0.0001, time = 0.1, dA = 2;
		^DetectSilence.ar(in, amp, time, dA)
	}
	*k { arg in = 0.0, amp = 0.0001, time = 0.1, dA = 2;
		^DetectSilence.kr(in, amp, time, dA)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

SI { // SoundIn
	*new { arg bus = 0, m=1.0, a=0.0;
		^SoundIn.ar(bus, m, a)
	}
	*ar { ^this.new }
}

LI { // LocalIn
	*new { arg numChannels = 1;
		^LocalIn.ar(numChannels)
	}
	*k { arg numChannels = 1;
		^LocalIn.kr(numChannels)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

LO { // LocalIn
	*new { arg channelsArray;
		^LocalOut.ar(channelsArray)
	}
	*k { arg channelsArray;
		^LocalOut.kr(channelsArray)
	}
	*ar { ^this.new }
	*kr { ^this.k }
}

Sp { // Splay
	*new { arg inArray, spread=1, level=1, center=0.0, levelComp=true;
		^Splay.ar(inArray, spread, level, center, levelComp)
	}
	*ar { ^this.new }
}

SpZ { // SplayZ
	*new { arg numChans=4, inArray, spread=1, level = 1, width = 2, center = 0.0;
		^SplayZ.ar(numChans, inArray, spread, level, width, center)
	*ar { ^this.new }
	}
}

SpAz { // SplayAz
	*new { arg numChans=4, inArray, spread=1, level = 1, width = 2, center = 0.0;
		^SplayAz.ar(numChans, inArray, spread, level, width, center)
	}
	*ar { ^this.new }
}

// extensions
+ Dictionary {
	// since Case Sensitivity is not supported in SC < 3.6 (check "oh" == "OH") in '==' I make my own
	findKeyForValueCS { arg argValue; 
		this.keysValuesArrayDo(array, { arg key, val, i;
			if (argValue.compare(val)==0, { ^key })
		});
		^nil
	}
}

+ SimpleNumber {
	w  { this.wait }	
	p  { this.postln }	
	l  { ^this.loop }	
	rd { arg aNumber=1.0, adverb; ^this.round(aNumber, adverb) }
	r  { ^this.rand }	
	rr { ^this.rrand }	
	mc {	^this.midicps }	
	cm { ^this.cpsmidi }	
	d  { ^this.distort }	
	t  { ^this.tanh }	
	c  { ^this.cubed }	
}

+ Function {
	p { this.play }	
	f { this.fork }	
}

+ SequenceableCollection {
	d  { ^this.distort }
	t  { ^this.tanh }
	r  { ^this.rand }
	rr  { ^this.rrand }
	c  { ^this.cubed }
	s  { ^this.squared }
	cl  { ^this.clip }
	cl2  { ^this.clip2 }
	mc {	^this.midicps }	
	// etc.
}

+ UGen {
 	r { arg lo = 0.0, hi = 1.0; // range
		^this.range(lo, hi);
 	} 	
 	c { arg lo = 0.0, hi = 1.0; // clip
		^this.clip(lo, hi);
 	}
 	c2 { arg num=1;
		^this.clip2(num);
 	}
 	rd {
		^this.round;	 	
	 }
 	t { 
		^this.tanh;
 	}
	d  { ^this.distort }
	ll { arg inMin, inMax, outMin, outMax, clip;
		^this.linlin(inMin, inMax, outMin, outMax, clip);
	}
	le { arg inMin, inMax, outMin, outMax, clip;
		^this.linexp(inMin, inMax, outMin, outMax, clip);
	}
	el { arg inMin, inMax, outMin, outMax, clip;
		^this.explin(inMin, inMax, outMin, outMax, clip);
	}
	ee { arg inMin, inMax, outMin, outMax, clip;
		^this.expexp(inMin, inMax, outMin, outMax, clip);
	}
}
