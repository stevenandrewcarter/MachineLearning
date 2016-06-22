using System.Collections.Generic;

namespace DigitsRecognizer {
  public interface IClassifier {
    void Train(IEnumerable<Observation> trainingSet);
    string Predict(int[] pixels);
  }
}
