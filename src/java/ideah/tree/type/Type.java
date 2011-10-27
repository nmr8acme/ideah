package ideah.tree.type;

import ideah.tree.Located;
import ideah.util.IRange;

public abstract class Type extends Located {

    protected Type(IRange location) {
        super(location);
    }
}
