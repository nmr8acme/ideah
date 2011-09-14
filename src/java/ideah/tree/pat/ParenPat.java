package ideah.tree.pat;

public final class ParenPat extends Pat {

    public final Pat pattern;

    public ParenPat(Pat pattern) {
        this.pattern = pattern;
    }
}
