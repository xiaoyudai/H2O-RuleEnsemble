/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-08-05T21:18:26.865Z
  3.10.0.3
  
  Standalone prediction code with sample test data for GBMModel named gbm

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://10.161.99.183:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://10.161.99.183:54321/3/Models.java/gbm > gbm.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m gbm.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="gbm", algorithm="gbm")
public class gbm extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Binomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 52; }
  public int nclasses() { return 2; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_gbm.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 2;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* sex */ gbm_ColInfo_0.VALUES,
    /* age */ null,
    /* address */ gbm_ColInfo_2.VALUES,
    /* famsize */ gbm_ColInfo_3.VALUES,
    /* Pstatus */ gbm_ColInfo_4.VALUES,
    /* Medu */ null,
    /* Fedu */ null,
    /* Mjob */ gbm_ColInfo_7.VALUES,
    /* Fjob */ gbm_ColInfo_8.VALUES,
    /* reason */ gbm_ColInfo_9.VALUES,
    /* nursery */ gbm_ColInfo_10.VALUES,
    /* internet */ gbm_ColInfo_11.VALUES,
    /* guardian.x */ gbm_ColInfo_12.VALUES,
    /* traveltime.x */ null,
    /* studytime.x */ null,
    /* failures.x */ null,
    /* schoolsup.x */ gbm_ColInfo_16.VALUES,
    /* famsup.x */ gbm_ColInfo_17.VALUES,
    /* paid.x */ gbm_ColInfo_18.VALUES,
    /* activities.x */ gbm_ColInfo_19.VALUES,
    /* higher.x */ gbm_ColInfo_20.VALUES,
    /* romantic.x */ gbm_ColInfo_21.VALUES,
    /* famrel.x */ null,
    /* freetime.x */ null,
    /* goout.x */ null,
    /* Dalc.x */ null,
    /* Walc.x */ null,
    /* health.x */ null,
    /* absences.x */ null,
    /* G1.x */ null,
    /* G2.x */ null,
    /* G3.x */ null,
    /* guardian.y */ gbm_ColInfo_32.VALUES,
    /* traveltime.y */ null,
    /* studytime.y */ null,
    /* failures.y */ null,
    /* schoolsup.y */ gbm_ColInfo_36.VALUES,
    /* famsup.y */ gbm_ColInfo_37.VALUES,
    /* paid.y */ gbm_ColInfo_38.VALUES,
    /* activities.y */ gbm_ColInfo_39.VALUES,
    /* higher.y */ gbm_ColInfo_40.VALUES,
    /* romantic.y */ gbm_ColInfo_41.VALUES,
    /* famrel.y */ null,
    /* freetime.y */ null,
    /* goout.y */ null,
    /* Dalc.y */ null,
    /* Walc.y */ null,
    /* health.y */ null,
    /* absences.y */ null,
    /* G1.y */ null,
    /* G2.y */ null,
    /* G3.y */ null,
    /* school */ gbm_ColInfo_52.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.8952879581151832,0.10471204188481675};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.8952879581151832,0.10471204188481675};

  public gbm() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(8840790183030741740L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    gbm_Forest_0.score0(data,preds);
    gbm_Forest_1.score0(data,preds);
    gbm_Forest_2.score0(data,preds);
    gbm_Forest_3.score0(data,preds);
    preds[2] = preds[1] + -2.145931282948669;
    preds[2] = 1/(1+Math.min(1.0E19, Math.exp(-preds[2])));
    preds[1] = 1.0-preds[2];
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.10925060898495084);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_gbm implements java.io.Serializable {
  public static final String[] VALUES = new String[52];
  static {
    NamesHolder_gbm_0.fill(VALUES);
  }
  static final class NamesHolder_gbm_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "sex";
      sa[1] = "age";
      sa[2] = "address";
      sa[3] = "famsize";
      sa[4] = "Pstatus";
      sa[5] = "Medu";
      sa[6] = "Fedu";
      sa[7] = "Mjob";
      sa[8] = "Fjob";
      sa[9] = "reason";
      sa[10] = "nursery";
      sa[11] = "internet";
      sa[12] = "guardian.x";
      sa[13] = "traveltime.x";
      sa[14] = "studytime.x";
      sa[15] = "failures.x";
      sa[16] = "schoolsup.x";
      sa[17] = "famsup.x";
      sa[18] = "paid.x";
      sa[19] = "activities.x";
      sa[20] = "higher.x";
      sa[21] = "romantic.x";
      sa[22] = "famrel.x";
      sa[23] = "freetime.x";
      sa[24] = "goout.x";
      sa[25] = "Dalc.x";
      sa[26] = "Walc.x";
      sa[27] = "health.x";
      sa[28] = "absences.x";
      sa[29] = "G1.x";
      sa[30] = "G2.x";
      sa[31] = "G3.x";
      sa[32] = "guardian.y";
      sa[33] = "traveltime.y";
      sa[34] = "studytime.y";
      sa[35] = "failures.y";
      sa[36] = "schoolsup.y";
      sa[37] = "famsup.y";
      sa[38] = "paid.y";
      sa[39] = "activities.y";
      sa[40] = "higher.y";
      sa[41] = "romantic.y";
      sa[42] = "famrel.y";
      sa[43] = "freetime.y";
      sa[44] = "goout.y";
      sa[45] = "Dalc.y";
      sa[46] = "Walc.y";
      sa[47] = "health.y";
      sa[48] = "absences.y";
      sa[49] = "G1.y";
      sa[50] = "G2.y";
      sa[51] = "G3.y";
    }
  }
}
// The class representing column sex
class gbm_ColInfo_0 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_0_0.fill(VALUES);
  }
  static final class gbm_ColInfo_0_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "F";
      sa[1] = "M";
    }
  }
}
// The class representing column address
class gbm_ColInfo_2 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_2_0.fill(VALUES);
  }
  static final class gbm_ColInfo_2_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "R";
      sa[1] = "U";
    }
  }
}
// The class representing column famsize
class gbm_ColInfo_3 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_3_0.fill(VALUES);
  }
  static final class gbm_ColInfo_3_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "GT3";
      sa[1] = "LE3";
    }
  }
}
// The class representing column Pstatus
class gbm_ColInfo_4 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_4_0.fill(VALUES);
  }
  static final class gbm_ColInfo_4_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "A";
      sa[1] = "T";
    }
  }
}
// The class representing column Mjob
class gbm_ColInfo_7 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    gbm_ColInfo_7_0.fill(VALUES);
  }
  static final class gbm_ColInfo_7_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "at_home";
      sa[1] = "health";
      sa[2] = "other";
      sa[3] = "services";
      sa[4] = "teacher";
    }
  }
}
// The class representing column Fjob
class gbm_ColInfo_8 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    gbm_ColInfo_8_0.fill(VALUES);
  }
  static final class gbm_ColInfo_8_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "at_home";
      sa[1] = "health";
      sa[2] = "other";
      sa[3] = "services";
      sa[4] = "teacher";
    }
  }
}
// The class representing column reason
class gbm_ColInfo_9 implements java.io.Serializable {
  public static final String[] VALUES = new String[4];
  static {
    gbm_ColInfo_9_0.fill(VALUES);
  }
  static final class gbm_ColInfo_9_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "course";
      sa[1] = "home";
      sa[2] = "other";
      sa[3] = "reputation";
    }
  }
}
// The class representing column nursery
class gbm_ColInfo_10 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_10_0.fill(VALUES);
  }
  static final class gbm_ColInfo_10_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column internet
class gbm_ColInfo_11 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_11_0.fill(VALUES);
  }
  static final class gbm_ColInfo_11_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column guardian.x
class gbm_ColInfo_12 implements java.io.Serializable {
  public static final String[] VALUES = new String[3];
  static {
    gbm_ColInfo_12_0.fill(VALUES);
  }
  static final class gbm_ColInfo_12_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "father";
      sa[1] = "mother";
      sa[2] = "other";
    }
  }
}
// The class representing column schoolsup.x
class gbm_ColInfo_16 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_16_0.fill(VALUES);
  }
  static final class gbm_ColInfo_16_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column famsup.x
class gbm_ColInfo_17 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_17_0.fill(VALUES);
  }
  static final class gbm_ColInfo_17_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column paid.x
class gbm_ColInfo_18 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_18_0.fill(VALUES);
  }
  static final class gbm_ColInfo_18_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column activities.x
class gbm_ColInfo_19 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_19_0.fill(VALUES);
  }
  static final class gbm_ColInfo_19_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column higher.x
class gbm_ColInfo_20 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_20_0.fill(VALUES);
  }
  static final class gbm_ColInfo_20_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column romantic.x
class gbm_ColInfo_21 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_21_0.fill(VALUES);
  }
  static final class gbm_ColInfo_21_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column guardian.y
class gbm_ColInfo_32 implements java.io.Serializable {
  public static final String[] VALUES = new String[3];
  static {
    gbm_ColInfo_32_0.fill(VALUES);
  }
  static final class gbm_ColInfo_32_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "father";
      sa[1] = "mother";
      sa[2] = "other";
    }
  }
}
// The class representing column schoolsup.y
class gbm_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_36_0.fill(VALUES);
  }
  static final class gbm_ColInfo_36_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column famsup.y
class gbm_ColInfo_37 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_37_0.fill(VALUES);
  }
  static final class gbm_ColInfo_37_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column paid.y
class gbm_ColInfo_38 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_38_0.fill(VALUES);
  }
  static final class gbm_ColInfo_38_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column activities.y
class gbm_ColInfo_39 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_39_0.fill(VALUES);
  }
  static final class gbm_ColInfo_39_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column higher.y
class gbm_ColInfo_40 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_40_0.fill(VALUES);
  }
  static final class gbm_ColInfo_40_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column romantic.y
class gbm_ColInfo_41 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_41_0.fill(VALUES);
  }
  static final class gbm_ColInfo_41_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "no";
      sa[1] = "yes";
    }
  }
}
// The class representing column school
class gbm_ColInfo_52 implements java.io.Serializable {
  public static final String[] VALUES = new String[2];
  static {
    gbm_ColInfo_52_0.fill(VALUES);
  }
  static final class gbm_ColInfo_52_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "GP";
      sa[1] = "MS";
    }
  }
}

class gbm_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += gbm_Tree_0_class_0.score0(fdata);
  }
}
class gbm_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[1]) || data[1 /* age */] <17.5f ? 
         (Double.isNaN(data[13]) || data[13 /* traveltime.x */] <1.5f ? 
            -0.011169591f : 
             (Double.isNaN(data[1]) || data[1 /* age */] <16.5f ? 
                -0.011169591f : 
                0.01846085f)) : 
         (Double.isNaN(data[13]) || data[13 /* traveltime.x */] <1.5f ? 
             (Double.isNaN(data[28]) || data[28 /* absences.x */] <7.5f ? 
                 (data[48 /* absences.y */] <0.5f ? 
                    -5.026316E-4f : 
                    0.031498242f) : 
                -0.011169591f) : 
            0.038609546f));
    return pred;
  } // constant pool size = 26B, number of visited nodes = 6, static init size = 0B
}


class gbm_Forest_1 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += gbm_Tree_1_class_0.score0(fdata);
  }
}
class gbm_Tree_1_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[1]) || data[1 /* age */] <17.5f ? 
         (Double.isNaN(data[1]) || data[1 /* age */] <16.5f ? 
            -0.011156599f : 
             (Double.isNaN(data[17]) || !GenModel.bitSetContains(GRPSPLIT0, 0, data[17 /* famsup.x */]) ? 
                 (Double.isNaN(data[39]) || !GenModel.bitSetContains(GRPSPLIT1, 0, data[39 /* activities.y */]) ? 
                     (data[31 /* G3.x */] <10.5f ? 
                        -0.011163687f : 
                        -0.0111741945f) : 
                    -4.8142474E-4f) : 
                0.010123996f)) : 
         (Double.isNaN(data[17]) || !GenModel.bitSetContains(GRPSPLIT2, 0, data[17 /* famsup.x */]) ? 
             (Double.isNaN(data[8]) || !GenModel.bitSetContains(GRPSPLIT3, 0, data[8 /* Fjob */]) ? 
                -0.011192628f : 
                -5.40053E-4f) : 
            0.038399793f));
    return pred;
  } // constant pool size = 50B, number of visited nodes = 7, static init size = 120B
  // {10000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT0 = new byte[] {1, 0, 0, 0};
  // {01000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT1 = new byte[] {2, 0, 0, 0};
  // {10000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT2 = new byte[] {1, 0, 0, 0};
  // {11011000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT3 = new byte[] {27, 0, 0, 0};
}


class gbm_Forest_2 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += gbm_Tree_2_class_0.score0(fdata);
  }
}
class gbm_Tree_2_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[1]) || data[1 /* age */] <17.5f ? 
         (Double.isNaN(data[1]) || data[1 /* age */] <16.5f ? 
            -0.011143767f : 
             (Double.isNaN(data[8]) || !GenModel.bitSetContains(GRPSPLIT0, 0, data[8 /* Fjob */]) ? 
                 (Double.isNaN(data[28]) || data[28 /* absences.x */] <7.5f ? 
                     (Double.isNaN(data[13]) || data[13 /* traveltime.x */] <1.5f ? 
                        -0.011154694f : 
                        -0.011186023f) : 
                    -4.5857741E-4f) : 
                 (data[51 /* G3.y */] <13.5f ? 
                    0.020633368f : 
                    -4.194927E-4f))) : 
         (Double.isNaN(data[33]) || data[33 /* traveltime.y */] <1.5f ? 
             (Double.isNaN(data[9]) || !GenModel.bitSetContains(GRPSPLIT1, 0, data[9 /* reason */]) ? 
                -0.01119446f : 
                0.020313509f) : 
             (Double.isNaN(data[2]) || !GenModel.bitSetContains(GRPSPLIT2, 0, data[2 /* address */]) ? 
                0.025759667f : 
                0.0702046f)));
    return pred;
  } // constant pool size = 53B, number of visited nodes = 9, static init size = 90B
  // {10010000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT0 = new byte[] {9, 0, 0, 0};
  // {10000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT1 = new byte[] {1, 0, 0, 0};
  // {10000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT2 = new byte[] {1, 0, 0, 0};
}


class gbm_Forest_3 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += gbm_Tree_3_class_0.score0(fdata);
  }
}
class gbm_Tree_3_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[1]) || data[1 /* age */] <17.5f ? 
         (Double.isNaN(data[1]) || data[1 /* age */] <16.5f ? 
            -0.011131093f : 
             (Double.isNaN(data[19]) || !GenModel.bitSetContains(GRPSPLIT0, 0, data[19 /* activities.x */]) ? 
                 (Double.isNaN(data[13]) || data[13 /* traveltime.x */] <1.5f ? 
                     (Double.isNaN(data[28]) || data[28 /* absences.x */] <7.5f ? 
                        -0.011140075f : 
                        -0.011153151f) : 
                    0.006545911f) : 
                 (Double.isNaN(data[48]) || data[48 /* absences.y */] <3.5f ? 
                    -0.005588551f : 
                    0.041759867f))) : 
         (data[33 /* traveltime.y */] <1.5f ? 
             (!GenModel.bitSetContains(GRPSPLIT1, 0, data[7 /* Mjob */]) ? 
                -0.011190945f : 
                0.018529605f) : 
             (Double.isNaN(data[9]) || !GenModel.bitSetContains(GRPSPLIT2, 0, data[9 /* reason */]) ? 
                0.016866596f : 
                0.06693003f)));
    return pred;
  } // constant pool size = 53B, number of visited nodes = 9, static init size = 90B
  // {01000000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT0 = new byte[] {2, 0, 0, 0};
  // {10011000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT1 = new byte[] {25, 0, 0, 0};
  // {10100000 00000000 00000000 00000000}
  public static final byte[] GRPSPLIT2 = new byte[] {5, 0, 0, 0};
}



