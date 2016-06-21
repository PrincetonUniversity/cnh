function [R F] = sub_perception(P,R,F,T,D);
   % PERCEPTION (nearest prey/pred from D matrix) 
   [R.Fd Fi] = min(D,[],1);
   R.i = find(R.Fd <= R.K); %<- all prey that can sense a pred
   R.j = Fi(R.i); %<- which pred is closest to those prey that can sense
   R.k = sub2ind(size(T),R.j,R.i);   

   [F.Rd Ri] = min(D,[],2);
   F.i = find(F.Rd <= F.K); %<- all pred that can sense a prey
   F.j = Ri(F.i); %<- which prey is closest to those pred that can sense
   F.k = sub2ind(size(T),F.i,F.j);

   % OPTIMAL PREY (avoidance)
   if isempty(R.i) == 0
   R.Avoid.U = zeros(size(R.x));
   R.Avoid.V = zeros(size(R.x));
   R.Avoid.U(R.i,1) = R.dmax .* sind(T(R.k)); % move u
   R.Avoid.V(R.i,1) = R.dmax .* cosd(T(R.k)); % move v
   else
   R.Avoid.U = zeros(size(R.x));
   R.Avoid.V = zeros(size(R.x));
   end

   % OPTIMAL PREDATION (attraction)
   if isempty(F.i) == 0
   F.Attract.U = zeros(size(F.x));
   F.Attract.V = zeros(size(F.x));
   F.Attract.U(F.i,1) = F.dmax .* sind(T(F.k)); % move u
   F.Attract.V(F.i,1) = F.dmax .* cosd(T(F.k)); % move v
   else
   F.Attract.U = zeros(size(F.x));
   F.Attract.V = zeros(size(F.x));
   end

   % PERCEPTION (alpha)
   R.Alpha = zeros(size(R.x));
   %R.Alpha(R.i) = 1; % Step function 
   R.Alpha(R.i) = ((-1./R.K).*R.Fd(R.i)) + 1;%<- perception is linear function of distance

   F.Alpha = zeros(size(F.x));
   %F.Alpha(F.i) = 1; % Step function
   F.Alpha(F.i) = ((-1./F.K).*F.Rd(F.i)) + 1;%<- perception is linear function of distance

