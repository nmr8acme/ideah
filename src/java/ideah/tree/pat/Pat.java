package ideah.tree.pat;

import ideah.tree.IRange;
import ideah.tree.Located;

public abstract class Pat extends Located {

    protected Pat(IRange location) {
        super(location);
    }
}
