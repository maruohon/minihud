package fi.dy.masa.minihud.data;

import java.util.Arrays;
import javax.annotation.Nullable;

import net.minecraft.entity.EnumCreatureType;

import malilib.util.MathUtils;

public class MobCapData
{
    protected static final int CAP_COUNT = MobCapDataHandler.ENTITY_CATEGORIES.length;
    protected static final int MAX_DATA_AGE_TICKS = 100; // TODO: move to a config option?

    protected final Cap[] data = createCapArray();
    protected final Cap[] stagingData = createCapArray();
    protected final boolean[] dataValid = new boolean[CAP_COUNT];
    protected final long[] worldTicks = new long[CAP_COUNT];
    protected boolean hasValidData;
    protected long completionWorldTick;

    public static Cap[] createCapArray()
    {
        Cap[] data = new Cap[CAP_COUNT];

        for (int i = 0; i < data.length; ++i)
        {
            data[i] = new Cap();
        }

        return data;
    }

    public boolean getHasValidData()
    {
        return this.hasValidData;
    }

    public boolean getHasRecentValidData(long worldTick)
    {
        return this.hasValidData && (worldTick - this.completionWorldTick <= MAX_DATA_AGE_TICKS);
    }

    public void clear()
    {
        this.clearStaging();
        this.hasValidData = false;
        this.completionWorldTick = -1L;
    }

    public Cap getCap(EntityCategory type)
    {
        return this.data[type.ordinal()];
    }

    public void setCurrentValue(EntityCategory type, int currentValue, long worldTick)
    {
        int index = type.ordinal();
        Cap cap = this.stagingData[index];
        cap.setCurrent(currentValue);
        this.checkCapComplete(index, cap, worldTick);
    }

    public void setCapValue(EntityCategory type, int capValue, long worldTick)
    {
        int index = type.ordinal();
        Cap cap = this.stagingData[index];
        cap.setCap(capValue);
        this.checkCapComplete(index, cap, worldTick);
    }

    public void setCurrentAndCapValues(EntityCategory type, int currentValue, int capValue, long worldTick)
    {
        int index = type.ordinal();

        this.stagingData[index].setCurrentAndCap(currentValue, capValue);
        this.dataValid[index] = true;
        this.worldTicks[index] = worldTick;

        this.checkStagingComplete(worldTick);
    }

    protected void clearStaging()
    {
        for (Cap data : this.stagingData)
        {
            data.setCurrentAndCap(-1, -1);
        }

        Arrays.fill(this.dataValid, false);
        Arrays.fill(this.worldTicks, -1);
    }

    protected void checkCapComplete(int index, Cap cap, long worldTick)
    {
        if (cap.getCurrent() >= 0 && cap.getCap() >= 0)
        {
            this.dataValid[index] = true;
            this.worldTicks[index] = worldTick;

            this.checkStagingComplete(worldTick);
        }
    }

    protected void checkStagingComplete(long worldTick)
    {
        for (boolean b : this.dataValid)
        {
            if (b == false)
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
                this.data[i].setFrom(this.stagingData[i]);
            }

            this.clearStaging();
        }

        this.hasValidData = true;
        this.completionWorldTick = worldTick;
    }

    public static class Cap
    {
        protected int current;
        protected int cap;

        public int getCurrent()
        {
            return this.current;
        }

        public int getCap()
        {
            return this.cap;
        }

        public boolean isFull()
        {
            return this.current >= this.cap;
        }

        public void setCurrent(int current)
        {
            this.current = current;
        }

        public void setCap(int cap)
        {
            this.cap = cap;
        }

        public void setCurrentAndCap(int current, int cap)
        {
            this.current = current;
            this.cap = cap;
        }

        public void setFrom(Cap other)
        {
            this.current = other.current;
            this.cap = other.cap;
        }
    }

    public enum EntityCategory
    {
        MONSTER ("monster",  EnumCreatureType.MONSTER),
        CREATURE("creature", EnumCreatureType.CREATURE),
        AMBIENT ("ambient",  EnumCreatureType.AMBIENT),
        WATER   ("water",    EnumCreatureType.WATER_CREATURE);

        private final EnumCreatureType vanillaCategory;
        private final String name;

        EntityCategory(String name, EnumCreatureType vanillaCategory)
        {
            this.name = name;
            this.vanillaCategory = vanillaCategory;
        }

        public String getName()
        {
            return this.name;
        }

        public EnumCreatureType getVanillaCategory()
        {
            return this.vanillaCategory;
        }

        public static EntityCategory fromVanillaCategory(EnumCreatureType type)
        {
            switch (type)
            {
                case MONSTER:           return MONSTER;
                case CREATURE:          return CREATURE;
                case AMBIENT:           return AMBIENT;
                case WATER_CREATURE:    return WATER;
                default:                return null;
            }
        }

        @Nullable
        public static EntityCategory fromVanillaCategoryName(String name)
        {
            switch (name)
            {
                case "monster":         return MONSTER;
                case "creature":        return CREATURE;
                case "ambient":         return AMBIENT;
                case "water_creature":  return WATER;
                default:                return null;
            }
        }
    }
}
