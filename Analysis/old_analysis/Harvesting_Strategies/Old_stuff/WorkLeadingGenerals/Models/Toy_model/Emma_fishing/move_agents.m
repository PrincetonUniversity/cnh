function [newx newy] = move_agents(oldx, oldy, prob_move)

% Moving agents
        dice1 = rand;   
        dice2 = rand;
        move_coordinates = [1 -1]; % my options in life
        
        if dice1 < prob_move
            ind_x = randi(2,1,1);  % create random index, can move +/- 1
            newx = oldx + move_coordinates(ind_x);
        else newx = oldx; % if I haven't moved, my coordinates stay the same
        end
        
        
        if dice2 < prob_move
           ind_y = randi(2,1,1);
           newy = oldy + move_coordinates(ind_y);
        else newy = oldy;
        end
end