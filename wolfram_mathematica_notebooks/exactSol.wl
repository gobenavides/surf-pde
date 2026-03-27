(* ::Package:: *)

BeginPackage["ExactSolution`"];


sinSolPoi::usage = "Args: x. Non-polynomial function depending only on space variables.";
BrandnerEtAlSteadyStokesVel::usage = "Args: n, P, x. Gamma-divergence-free solution for the fixed-surface Stokes problem from Brandner et al, 2022. Velocity.";
BrandnerEtAlSteadyStokesPres::usage = "Args: x. Gamma-divergence-free solution for the fixed-surface Stokes problem from Brandner et al, 2022. Pressure.";

OlshanskiiEtAlSteadyStokesVel::usage = "Args: P, x. Non-solenoidal solution for the fixed-surface Stokes problem from Olshanskii et al, 2018. Velocity.";
OlshanskiiEtAlSteadyStokesPres::usage = "Args: x. Non-solenoidal solution for the fixed-surface Stokes problem from Olshanskii et al, 2018. Pressure.";


Begin["`Private`"];


<<"surfDiffOps.wl";
sinSolPoi[x_]:=Sin[Pi*x[[1]]]*Sin[Pi*x[[2]]*x[[3]]];

BrandnerEtAlSteadyStokesVel[n_,P_,x_]:=FullSimplify@gammaCurlScalar[n,P,x[[1]]^2*x[[2]]-5*x[[3]]^3,x];
BrandnerEtAlSteadyStokesPres[x_]:=x[[1]]^3+x[[1]]*x[[2]]*x[[3]];

OlshanskiiEtAlSteadyStokesVel[P_,x_]:=Dot[P,{-x[[3]]^2,x[[2]],x[[1]]}];
OlshanskiiEtAlSteadyStokesPres[x_]:=x[[1]]*x[[2]]^3+x[[3]];


End[];


EndPackage[];
