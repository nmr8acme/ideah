package ideah.tree.stmt;

import ideah.tree.Located;
import ideah.util.IRange;

public abstract class Statement extends Located {

    protected Statement(IRange location) {
        super(location);
    }
}
