% Function to bias my movement towards fish if I can see them

function [newx newy] = chase_fish(oldx, oldy, fish_view)
        
        [distances] = sqrt(fish_view(:,1).^2 + fish_view(:,2).^2); % more pythagoras to calculate distances (know x and y distance from fish)

        mindist_ind = find(distances==min(distances));  % choose the shortest distance
        selected_fish = mindist_ind(randi(length(mindist_ind),1,1)); % if there are equidistant fish, pick one at random
        
        closest_fish(1) = fish_view(selected_fish,1); % location of closest fish
        closest_fish(2) = fish_view(selected_fish,2);
        
        if closest_fish(1) > 0  % if fish is to the right on x axis, take step to right
            newx = oldx + 1;
        elseif closest_fish(1) < 0  % if fish to the left on x axis, take step to left
            newx = oldx - 1;
        else newx = oldx;   % else don't move
        end
        
        if closest_fish(2) > 0
            newy = oldy + 1;
        elseif closest_fish(2) < 0
            newy = oldy - 1;
        else newy = oldy;
        end
        
end