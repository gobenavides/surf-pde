(* ::Package:: *)

BeginPackage["Levelsets`"];


phiSphere::usage = "phiSphere[R, x] returns levelset of a sphere of radius R, correctly scaled.";
phiTorus::usage = "phiTorus[r, R, x] returns levelset of a torus with radii r, R, for computation.";
phiCheese::usage = "phiCheese[x] returns levelset of a cheese surface of genus 6, for computation.";
phiBiconc::usage = "phiBiconc[d,c,x] returns levelset of a biconcave shape with parameters d,c, for computation.";
phiDziuk::usage = "phiDziuk[x] returns levelset of a Dziuk surface, for computation.";
phiSphereComp::usage = "phiSphereComp[R, x] returns levelset of a sphere of radius R, for computation.";


Begin["`Private`"];


phiSphere[R_,x_]:=Sqrt[x[[1]]^2 + x[[2]]^2 + x[[3]]^2] - R;
phiTorus[r_,R_,x_]:=(Sqrt[x[[1]]^2 + x[[2]]^2] - R)^2 + x[[3]]^2 - r^2;
phiCheese[x_]:=2*x[[2]]*(x[[2]]^2-3x[[1]]^2)*(1-x[[3]]^2)+(x[[1]]^2+x[[2]]^2)^2-(9*x[[3]]^2-1)*(1-x[[3]]^2);
phiBiconc[d_,c_,x_]:=(d^2+x[[1]]^2+x[[2]]^2+x[[3]]^2)^3-8d^2*(x[[2]]^2+x[[3]]^2)-c^4;
phiDziuk[x_]:=1/4*x[[1]]^2+x[[2]]^2+(4*x[[3]]^2)/((1+1/2*Sin[Pi*x[[1]]])^2)-1/2;

phiSphereComp[R_,x_]:=x[[1]]^2 + x[[2]]^2 + x[[3]]^2 - R^2;


End[];


EndPackage[];
