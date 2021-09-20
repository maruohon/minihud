package fi.dy.masa.minihud.data;

import java.util.Arrays;
import net.minecraft.entity.EnumCreatureType;
import fi.dy.masa.malilib.util.MathUtils;

public class MobCapData
{
    private final Cap[] data = createCapArray();
    private final Cap[] stagingData = createCapArray();
    private final boolean[] dataValid = new boolean[MobCapDataHolder.CREATURE_TYPES.length];
    private final long[] worldTicks = new long[MobCapDataHolder.CREATURE_TYPES.length];
    private boolean hasValidData;

    public static Cap[] createCapArray()
    {
        Cap[] data = new Cap[MobCapDataHolder.CREATURE_TYPES.length];

        for (int i = 0; i < data.length; ++i)
        {
            data[i] = new Cap();
        }

        return data;
    }

    public void clear()
    {
        this.clearStaging();
        this.hasValidData = false;
    }

    private void clearStaging()
    {
        for (Cap data : this.stagingData)
        {
            data.setCurrentAndCap(-1, -1);
        }

        Arrays.fill(this.dataValid, false);
        Arrays.fill(this.worldTicks, -1);
    }

    public void setCapValue(EnumCreatureType type, boolean isCap, int value, long worldTick)
    {
        int index = type.ordinal();
        Cap cap = this.stagingData[index];

        if (isCap)
        {
            cap.setCap(value);
        }
        else
        {
            cap.setCurrent(value);
        }

        this.checkCapComplete(index, cap, worldTick);
    }

    private void checkCapComplete(int index, Cap cap, long worldTick)
    {
        if (cap.getCurrent() >= 0 && cap.getCap() >= 0)
        {
            this.dataValid[index] = true;
            this.worldTicks[index] = worldTick;

            this.checkStagingComplete();
        }
    }

    public void setCapValues(EnumCreatureType type, int current, int cap, long worldTick)
    {
        int index = type.ordinal();

        this.stagingData[index].setCurrentAndCap(current, cap);
        this.dataValid[index] = true;
        this.worldTicks[index] = worldTick;

        this.checkStagingComplete();
    }

    public boolean getHasValidData()
    {
        return this.hasValidData;
    }

    public Cap getCap(EnumCreatureType type)
    {
        return this.data[type.ordinal()];
    }

    private void checkStagingComplete()
    {
        for (int i = 0; i < this.dataValid.length; ++i)
        {
            if (this.dataValid[i] == false)
            {
                return;
            }
        }

        long min = MathUtils.getMinValue(this.worldTicks);
        long max = MathUtils.getMaxValue(this.worldTicks);

        // Require all the values to have been received within 60 ticks
        // of each other for the data set to be considered valid
        if (max - min <= 60)
        {
            for (int i = 0; i < this.stagingData.length; ++i)
            {
                Cap cap = this.stagingData[i];
                this.data[i].setCurrentAndCap(cap.getCurrent(), cap.getCap());
            }

            this.clearStaging();
        }

        this.hasValidData = true;
    }
}
