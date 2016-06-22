% Produces the velocity vectors (R.ui, R.vi, F.ui, F.vi)
% for movement
function [R F] = sub_movement(P,R,F);

   % MOVEMENT
   R.u = ((1-R.Alpha).*((R.u.*(1-(P.dt./R.tau))) + (R.sig_u.*sqrt((2.*P.dt)./R.tau).*randn(size(R.x,1),1)))) +...
         ((R.Alpha)   .* (R.Avoid.U 		  + (R.sig_u.*sqrt((2.*P.dt)./R.tau).*randn(size(R.x,1),1))));
   R.v = ((1-R.Alpha).*((R.v.*(1-(P.dt./R.tau))) + (R.sig_v.*sqrt((2.*P.dt)./R.tau).*randn(size(R.x,1),1)))) +...
         ((R.Alpha)   .* (R.Avoid.V 		  + (R.sig_v.*sqrt((2.*P.dt)./R.tau).*randn(size(R.x,1),1))));

   F.u = ((1-F.Alpha).*((F.u.*(1-(P.dt./F.tau))) + (F.sig_u.*sqrt((2.*P.dt)./F.tau).*randn(size(F.x,1),1)))) +...
         ((F.Alpha)   .*(F.Attract.U 		  + (F.sig_u.*sqrt((2.*P.dt)./F.tau).*randn(size(F.x,1),1))));
   F.v = ((1-F.Alpha).*((F.v.*(1-(P.dt./F.tau))) + (F.sig_v.*sqrt((2.*P.dt)./F.tau).*randn(size(F.x,1),1)))) +...
         ((F.Alpha)   .*(F.Attract.V 		  + (F.sig_v.*sqrt((2.*P.dt)./F.tau).*randn(size(F.x,1),1))));

