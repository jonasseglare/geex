public class BBox {

    public static float[] computeBBox(float[] data) {
        long n = data.length/4;
        float[] result = new float[6];

        float minx = data[0];
        float maxx = data[0];
        float miny = data[1];
        float maxy = data[1];
        float minz = data[2];
        float maxz = data[2];
        for (int i = 0; i < n; i += 4) {
            float x = data[i + 0];
            float y = data[i + 1];
            float z = data[i + 2];
            if (java.lang.Float.isFinite(x) &&
                java.lang.Float.isFinite(y) &&
                java.lang.Float.isFinite(z)) {

                minx = Math.min(minx, x);
                maxx = Math.max(maxx, x);

                miny = Math.min(miny, y);
                maxy = Math.max(maxy, y);

                minz = Math.min(minz, z);
                maxz = Math.max(maxz, z);
            }
        }

        result[0] = minx;
        result[1] = maxx;
        result[2] = miny;
        result[3] = maxy;
        result[4] = minz;
        result[5] = maxz;

        return result;
    }

};
