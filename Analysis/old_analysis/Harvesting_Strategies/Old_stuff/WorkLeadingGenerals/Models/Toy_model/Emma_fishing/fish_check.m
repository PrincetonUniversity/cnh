% Function to perceive fish

function [fish_view] = fish_check(fish_radius, world, Li, worldsize)

% define rough area to look in 
[check_area] = [Li(:) - fish_radius, Li(:) + fish_radius]; % first row gives x range, second row gives y range

j = 1; % counter for fish_view store
fish_view = zeros(1,2);

 for x = check_area(1,1):check_area(1,2)
     for y = check_area(2,1):check_area(2,2)
         if x < 1                       % if the x limit is < 0, means that it needs to start on other side of periodic boundary
             real_x = worldsize + x;
         elseif x > worldsize
             real_x = x - worldsize;    % same here, but opposite direction
         else real_x = x;
             
         end
         if y < 1
             real_y = worldsize + y;
         elseif y > worldsize
             real_y = y - worldsize;
         else real_y = y;
             
         end
         
         if world(real_y,real_x) == 1 && sqrt((Li(1)-x).^2 + (Li(2)-y).^2) < fish_radius % a little bit of pythagoras to get our circular zone
             fish_view(j,:) = [x-Li(1) y-Li(2)]; % gives me an index of all fish locations in terms of x and y relative to me
             j = j + 1; % increment counter
         end
     end
 end
 
end
 
 
