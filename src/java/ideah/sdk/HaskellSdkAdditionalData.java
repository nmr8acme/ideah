package ideah.sdk;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.projectRoots.SdkAdditionalData;
import com.intellij.openapi.projectRoots.SdkModel;
import org.jdom.Element;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HaskellSdkAdditionalData implements SdkAdditionalData {

    @NonNls
    private static final String JDK = "jdk";
    @NonNls
    private static final String BUILD_TARGET = "sdk";

    private String myJavaSdkName;
    private final Sdk myAndroidSdk;
    private Sdk myJavaSdk;

    // hash string
    private String myBuildTarget;

//  private AndroidPlatform myAndroidPlatform = null;

    public HaskellSdkAdditionalData(@NotNull Sdk androidSdk, Sdk javaSdk) {
        myJavaSdk = javaSdk;
        myAndroidSdk = androidSdk;
    }

    public HaskellSdkAdditionalData(@NotNull Sdk androidSdk, @NotNull Element element) {
        myAndroidSdk = androidSdk;
        myJavaSdkName = element.getAttributeValue(JDK);
        myBuildTarget = element.getAttributeValue(BUILD_TARGET);
    }

    public HaskellSdkAdditionalData(Sdk androidSdk) {
        myAndroidSdk = androidSdk;
    }

    public void checkValid(SdkModel sdkModel) throws ConfigurationException {
        if (getJavaSdk() == null) {
//      throw new ConfigurationException(AndroidBundle.message("android.sdk.configure.jdk.error"));
            throw new ConfigurationException("android.sdk.configure.jdk.error");
        }
    }

    public Object clone() throws CloneNotSupportedException {
        HaskellSdkAdditionalData data = (HaskellSdkAdditionalData) super.clone();
        data.setJavaSdk(getJavaSdk());
        data.myBuildTarget = myBuildTarget;
        return data;
    }

    @Nullable
    public Sdk getJavaSdk() {
        final ProjectJdkTable jdkTable = ProjectJdkTable.getInstance();
        if (myJavaSdk == null) {
            if (myJavaSdkName != null) {
                myJavaSdk = jdkTable.findJdk(myJavaSdkName);
                myJavaSdkName = null;
            } else {
                for (Sdk jdk : jdkTable.getAllJdks()) {
//          if (AndroidSdkUtils.isApplicableJdk(jdk)) {
                    myJavaSdk = jdk;
                    break;
//          }
                }
            }
        }
        return myJavaSdk;
    }

    public void setJavaSdk(final Sdk javaSdk) {
        myJavaSdk = javaSdk;
    }

//  public void setBuildTarget(IAndroidTarget target) {
//    myBuildTarget = target.hashString();
//  }

    public void save(Element element) {
        final Sdk sdk = getJavaSdk();
        if (sdk != null) {
            element.setAttribute(JDK, sdk.getName());
        }
        if (myBuildTarget != null) {
            element.setAttribute(BUILD_TARGET, myBuildTarget);
        }
    }

//  @Nullable
//  public IAndroidTarget getBuildTarget(@NotNull AndroidSdk sdkObject) {
//    return myBuildTarget != null ? sdkObject.findTargetByHashString(myBuildTarget) : null;
//  }

//  @Nullable
//  public AndroidPlatform getAndroidPlatform() {
//    if (myAndroidPlatform == null) {
//      myAndroidPlatform = AndroidPlatform.parse(myAndroidSdk);
//    }
//    return myAndroidPlatform;
//  }
}
