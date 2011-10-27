package ideah.tree.pat;

import ideah.tree.Located;

import java.util.List;

// todo: record pattern
public final class ConPatDetails {

    public final List<Pat> patterns;

    public ConPatDetails(List<Pat> patterns) {
        this.patterns = patterns;
    }

    public Iterable<? extends Located> getChildren() {
        return patterns;
    }
}
