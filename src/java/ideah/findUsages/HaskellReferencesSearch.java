package ideah.findUsages;

import com.intellij.openapi.application.QueryExecutorBase;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import ideah.psi.impl.HPIdentImpl;
import ideah.util.CompilerLocation;
import ideah.util.DeclarationPosition;
import ideah.util.LineCol;
import ideah.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

public final class HaskellReferencesSearch extends QueryExecutorBase<PsiReference, ReferencesSearch.SearchParameters> {

    private static final Logger LOG = Logger.getInstance("ideah.findUsages.HaskellReferencesSearch");

    public HaskellReferencesSearch() {
        super(true);
    }

    @Override
    public void processQuery(@NotNull ReferencesSearch.SearchParameters queryParameters, @NotNull Processor<PsiReference> consumer) {
        PsiElement element = queryParameters.getElementToSearch();
        if (element instanceof HPIdentImpl) {
            HPIdentImpl ident = (HPIdentImpl) element;
            PsiFile file = element.getContainingFile();
            try {
                DeclarationPosition declaration = DeclarationPosition.get(file, LineCol.fromOffset(file, element.getTextOffset()));
                LineCol coord = declaration.coord;
                VirtualFile virtualFile = file.getVirtualFile();
                Project project = file.getProject();
                Module module = ProjectRootManager.getInstance(project).getFileIndex().getModuleForFile(virtualFile);
                CompilerLocation compiler = CompilerLocation.get(module);
                ProcessLauncher launcher = new ProcessLauncher(true, null,
                    compiler.exe,
                    "-m", "FindUsages",
                    "-g", compiler.libPath,
                    "-s", CompilerLocation.rootsAsString(module, false),
                    "--line-number", String.valueOf(coord.line), "--column-number", String.valueOf(coord.column),
                    virtualFile.getPath()
                );
                List<String> referenceStrings = new ArrayList<String>();
                BufferedReader bf = new BufferedReader(new StringReader(launcher.getStdOut()));
                String line = bf.readLine();
                while (line != null) {
                    referenceStrings.add(line);
                    line = bf.readLine();
                }
                Iterator<String> refIterator = referenceStrings.iterator();
                while (refIterator.hasNext()) {
                    String lineColStr = refIterator.next();
                    LineCol refLineCol = LineCol.parse(lineColStr);
                    if (refIterator.hasNext() && refLineCol != null) {
                        String refModuleStr = refIterator.next();
                        consumer.process(HPIdentImpl.getElementAt(project, new DeclarationPosition(refLineCol, refModuleStr)).getReference());
                    }
                }
            } catch (Exception e) {
                LOG.error(e);
            }
            consumer.process(ident);
            //GroovyConstructorUsagesSearcher.processConstructorUsages(ident, queryParameters.getScope(), consumer, queryParameters.getOptimizer(), true, false);
        }
    }
}
