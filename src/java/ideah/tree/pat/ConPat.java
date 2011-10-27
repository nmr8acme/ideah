package ideah.tree.pat;

import com.google.common.collect.Iterables;
import ideah.tree.Ident;
import ideah.tree.Located;
import ideah.util.IRange;

import java.util.Arrays;

public final class ConPat extends Pat {

    public final Ident constructor;
    public final ConPatDetails details;

    public ConPat(IRange location, Ident constructor, ConPatDetails details) {
        super(location);
        this.constructor = constructor;
        this.details = details;
    }

    protected Iterable<Located> getChildren() {
        return Iterables.concat(Arrays.asList(constructor), details.getChildren());
    }
}
