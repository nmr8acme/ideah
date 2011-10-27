package ideah.tree.pat;

import ideah.tree.Located;
import ideah.util.IRange;

public abstract class Pat extends Located {

    protected Pat(IRange location) {
        super(location);
    }
}
