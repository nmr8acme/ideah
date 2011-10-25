package ideah.tree.pat;

import ideah.tree.Located;
import ideah.util.LineColRange;

public abstract class Pat extends Located {

    protected Pat(LineColRange location) {
        super(location);
    }
}
