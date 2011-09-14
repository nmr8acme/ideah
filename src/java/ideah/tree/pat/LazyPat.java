package ideah.tree.pat;

public final class LazyPat extends Pat {

    public final Pat pattern;

    public LazyPat(Pat pattern) {
        this.pattern = pattern;
    }
}
