package ideah.imports;

import com.intellij.lang.ImportOptimizer;
import com.intellij.openapi.compiler.CompilerMessageCategory;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.util.EmptyRunnable;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiFile;
import ideah.compiler.GHCMessage;
import ideah.compiler.LaunchGHC;
import ideah.parser.HaskellFile;
import ideah.util.CompilerLocation;
import ideah.util.DeclarationPosition;
import ideah.util.LineColRange;
import ideah.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HaskellImportOptimizer implements ImportOptimizer {

    private static final Logger LOG = Logger.getInstance("ideah.imports.HaskellImportOptimizer");

    @Override
    public boolean supports(PsiFile psiFile) {
        return psiFile instanceof HaskellFile;
    }

    @NotNull
    @Override
    public Runnable processFile(final PsiFile psiFile) {
        Runnable empty = EmptyRunnable.getInstance();
        final String path = psiFile.getVirtualFile().getPath();
        Module module = DeclarationPosition.getDeclModule(psiFile);
        List<GHCMessage> ghcMessages = LaunchGHC.compile(null, path, module, true);
        final List<Import> redundantImports = getRedundantImports(ghcMessages);
        if (redundantImports.isEmpty())
            return empty;
        final CompilerLocation compiler = CompilerLocation.get(module);
        if (compiler == null)
            return empty;
        return new Runnable() {

            @Override
            public void run() {
                final Map<Import, LineColRange> importRanges = getImportRanges(compiler, path);
                Collections.sort(redundantImports, new Comparator<Import>() {
                    @Override
                    public int compare(Import i1, Import i2) {
                        return importRanges.get(i2).end.compareTo(importRanges.get(i1).end);
                    }
                });
                for (Import redImp : redundantImports) {
                    LineColRange range = importRanges.get(redImp);
                    if (range != null) {
                        removeImport(range);
                    }
                }
            }

            private void removeImport(LineColRange range) {
                PsiDocumentManager manager = PsiDocumentManager.getInstance(psiFile.getProject());
                Document document = manager.getDocument(psiFile);
                if (document == null)
                    return;
                manager.commitDocument(document);
                TextRange textRange = range.getRange(psiFile);
                int start = textRange.getStartOffset();
                int end = textRange.getEndOffset();
                int line = document.getLineNumber(end);
                int lineStart = document.getLineStartOffset(line);
                int lineEnd = document.getLineEndOffset(line);
                String functionName = document.getText(textRange);
                String importLine = document.getText().substring(lineStart, lineEnd);
                int rightWhites = getWhites(Pattern.compile(".*" + functionName + "([ ]*,[ ]*).*"), importLine);
                if (rightWhites == 0) {
                    start -= getWhites(Pattern.compile(".*(,[ ]*)" + functionName + ".*"), importLine);
                } else {
                    end += rightWhites;
                }
                boolean isEndOfLine = document.getLineEndOffset(line) == end;
                int endOffset = isEndOfLine ? end + document.getLineSeparatorLength(line) : end;
                document.replaceString(start, endOffset, "");
            }
        };
    }

    private static int getWhites(Pattern whitespacePattern, String importLine) {
        Matcher whitespaceMatcher = whitespacePattern.matcher(importLine);
        if (whitespaceMatcher.matches())
            return whitespaceMatcher.group(1).length();
        return 0;
    }

    private Map<Import, LineColRange> getImportRanges(CompilerLocation compiler, String path) { // todo ugly and terrible
        List<String> args = compiler.getCompileOptionsList(
            "-m", "AllImports",
            path
        );
        Map<Import, LineColRange> ranges = new HashMap<Import, LineColRange>();
        try {
            ProcessLauncher launcher = new ProcessLauncher(true, null, args);
            BufferedReader bf = new BufferedReader(new StringReader(launcher.getStdOut()));
            String line = bf.readLine();
            while (line != null) {
                if (line.startsWith(ProcessLauncher.NEW_MSG_INDICATOR)) {
                    String moduleName = bf.readLine();
                    if (moduleName == null)
                        break;
                    String mrange = bf.readLine();
                    if (mrange == null)
                        break;
                    LineColRange mLineColRange = new LineColRange(mrange);
                    line = bf.readLine();
                    while (line != null && !ProcessLauncher.NEW_MSG_INDICATOR.equals(line)) {
                        String functionName = line;
                        String frange = bf.readLine();
                        if (frange == null)
                            break;
                        LineColRange fLineColRange = new LineColRange(frange);
                        ranges.put(new Import(moduleName, functionName), fLineColRange);
                        line = bf.readLine();
                    }
                    ranges.put(new Import(moduleName), mLineColRange);
                }
            }
        } catch (Exception e) {
            LOG.error(e);
        }
        return ranges;
    }

    private List<Import> getRedundantImports(List<GHCMessage> ghcMessages) {
        List<Import> redundantImports = new ArrayList<Import>();
        for (GHCMessage ghcMessage : ghcMessages) {
            if (ghcMessage.getCategory() == CompilerMessageCategory.WARNING) {
                String errorMessage = ghcMessage.getErrorMessage();
                Matcher moduleWarnMatcher = Pattern.compile("The (qualified )?import of `([^ ]+)' is redundant.*", Pattern.DOTALL).matcher(errorMessage);
                if (moduleWarnMatcher.matches()) {
                    redundantImports.add(new Import(moduleWarnMatcher.group(2)));
                } else {
                    Matcher functionWarnMatcher = Pattern.compile("The import of `(.+)'[ \n]+from module `([^ ]+)' is redundant.*", Pattern.DOTALL).matcher(errorMessage);
                    if (functionWarnMatcher.matches()) {
                        String functionGroup = functionWarnMatcher.group(1);
                        String module = functionWarnMatcher.group(2);
                        String[] functions = functionGroup.split(",[ \n]+");
                        if (functions.length == 0) {
                            redundantImports.add(new Import(module, functionGroup));
                        } else {
                            for (String function : functions) {
                                redundantImports.add(new Import(module, function));
                            }
                        }
                    }
                }
            }
        }
        return redundantImports;
    }

    private class Import {
        final String module;
        final String function;

        private Import(@NotNull String module) {
            this(module, null);
        }

        private Import(@NotNull String module, String function) {
            this.module = module;
            this.function = function;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Import anImport = (Import) o;
            if (function != null ? !function.equals(anImport.function) : anImport.function != null) return false;
            return module.equals(anImport.module);

        }

        @Override
        public int hashCode() {
            int result = module.hashCode();
            result = 31 * result + (function != null ? function.hashCode() : 0);
            return result;
        }
    }
}
