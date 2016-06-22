using System;
using System.Collections.Generic;

namespace DigitsRecognizer {
  class BasicClassifier : IClassifier {
    private IEnumerable<Observation> data;
    private readonly IDistance distance;

    public BasicClassifier(IDistance distance) {
      this.distance = distance;
    }

    public string Predict(int[] pixels) {
      Observation currentBest = null;
      var shortest = double.MaxValue;
      foreach (var observation in data) {
        var dist = distance.Between(observation.Pixels, pixels);
        if (dist < shortest) {
          shortest = dist;
          currentBest = observation;
        }        
      }
      return currentBest.Label;
    }

    public void Train(IEnumerable<Observation> trainingSet) {
      this.data = trainingSet;
    }
  }
}
