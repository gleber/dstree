-record(forward,  {origin, sender, visited, depth}).
-record(finished, {origin, sender, tree}).
-record(return,   {origin, sender, subtree, visited}).
-record(timeout,  {origin, sender, dead, attempt = 0, ref, visited}).
-record(working,  {origin, sender, waiting_for, visited}).
