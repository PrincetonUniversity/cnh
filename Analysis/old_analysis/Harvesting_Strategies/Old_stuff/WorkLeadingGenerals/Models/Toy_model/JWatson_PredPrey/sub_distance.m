% Rad optimized code to get distances between cartesian points
% Don't try this at home, hyper dimensional and confusing
% Inputs: F.x F.y, R.x R.y
% Outputs: D (distance matrix), T (angle matrix)
function [T D] = sub_distance(P,R,F);

   %p = [1 1; 3 1];
   %n = [49 1;2 1];
   p = [F.x F.y];
   n = [R.x R.y];

   n = reshape(n',[1 2 size(n,1) 1]);
   n = repmat(n,[size(P.GHOSTS,1) 1 1]) + repmat(P.GHOSTS,[1 1 size(n,3)]);
   n = repmat(n,[1 1 1 size(p,1)]);

   p = reshape(p',[1 2 size(p,1)]);
   p = repmat(p,[size(n,1) 1 1]);
   p = reshape(p,[size(p,1) 2 1 size(n,4)]);
   p = repmat(p,[1 1 size(n,3) 1]);

   d = sqrt(sum((p-n).^2,2));
   [D i] = min(d,[],1);
   D = squeeze(D(1,1,:,:))'; %Distance

   T = (p-n);
   T = radtodeg(atan2(T(:,1,:),T(:,2,:)));
   T = squeeze(T(:,1,:));
   j = squeeze(i); j = [j(:) [1:length(j(:))]'];
   j = sub2ind(size(T),j(:,1),j(:,2));
   T = T(j);
   T = mod(T-180,360);
   T = reshape(T,size(D'))';

