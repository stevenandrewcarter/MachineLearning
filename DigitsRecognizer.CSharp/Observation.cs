namespace DigitsRecognizer {
  public class Observation {
    public Observation(string label, int[] pixels) {
      Label = label;
      Pixels = pixels;
    }

    public string Label { get; set; }
    public int[] Pixels { get; set; }
  }
}
