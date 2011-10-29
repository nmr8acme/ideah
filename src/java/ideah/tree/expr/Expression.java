package ideah.tree.expr;

import ideah.tree.IRange;
import ideah.tree.Located;

public abstract class Expression extends Located {

    protected Expression(IRange location) {
        super(location);
    }
}
