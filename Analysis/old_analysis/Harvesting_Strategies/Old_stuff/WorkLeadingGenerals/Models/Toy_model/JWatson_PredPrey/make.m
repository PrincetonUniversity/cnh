
clear; close all;

% Global parameters
 P.dt    = 1; % time step
 P.Dx  = 20;  % domain size
 P.Dy  = 20;

% GHOSTS FOR PERIODIC PERCEPTION
P.GHOSTS = [-P.Dx -P.Dy;...
       0     -P.Dy;...
       +P.Dx -P.Dy;...
       -P.Dx 0;...
       0     0;...
       +P.Dx 0;...
       -P.Dx +P.Dy;...
       0     +P.Dy;...
       +P.Dx +P.Dy];

% Prey Parameters
 R.N      = 400;   %<- # of prey
 R.x(1:R.N,1) = P.Dx .* rand(R.N,1);%<- initial x
 R.y(1:R.N,1) = P.Dy .* rand(R.N,1);%<- initial y
 R.U      = 0;%<- mean u speed of prey
 R.V      = 0;%<- mean v speed of prey
 R.sig_u  = .2;%<- s.d. u
 R.sig_v  = .2;%<- s.d. v
 R.u      = zeros(R.N,1); %<- initial velocity
 R.v      = zeros(R.N,1); %<- initial velocity
 R.tau    = 5;%<- decorrelation time scale
 R.K      = 10;%<- distance at which prey can start seeing pred
 R.dmax   = sqrt((R.u(1)+R.sig_u).^2 + (R.v(1)+R.sig_v).^2); %<- max step

% Pred Parameters
 F.N      = 40;   %<- # of prey
 F.x(1:F.N,1) = P.Dx .* rand(F.N,1);%<- initial x
 F.y(1:F.N,1) = P.Dy .* rand(F.N,1);%<- initial y
 F.U      = 0;%<- u speed of prey
 F.V      = 0;%<- v speed of prey
 F.sig_u  = .3;%<- s.d. u
 F.sig_v  = .3;%<- s.d. v
 F.u      = zeros(F.N,1); %<- initial velocity
 F.v      = zeros(F.N,1); %<- initial velocity
 F.tau    = 10;	%<- decorrelation time scale
 F.K      = 15;%<- distance at which prey can start seeing pred
 F.G      = 1;%<- distance at which pred can eat prey
 F.dmax   = sqrt((F.u(1)+F.sig_u).^2 + (F.v(1)+F.sig_v).^2); %<- max step

% Spin up
P = sub_hunt(P,R,F);


