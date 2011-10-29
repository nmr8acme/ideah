package ideah.tree.pat;

import ideah.tree.IRange;
import ideah.tree.Ident;
import ideah.tree.Located;

import java.util.Arrays;

public final class AsPat extends Pat {

    public final Ident name;
    public final Pat pattern;

    public AsPat(IRange location, Ident name, Pat pattern) {
        super(location);
        this.name = name;
        this.pattern = pattern;
    }

    protected Iterable<Located> getChildren() {
        return Arrays.asList(name, pattern);
    }
}
