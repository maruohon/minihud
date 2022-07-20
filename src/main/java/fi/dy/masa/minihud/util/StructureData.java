package fi.dy.masa.minihud.util;

import java.util.Iterator;
import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;
import net.minecraft.nbt.NbtCompound;
import net.minecraft.nbt.NbtList;
import net.minecraft.structure.StructurePiece;
import net.minecraft.structure.StructureStart;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.malilib.util.IntBoundingBox;

public class StructureData
{
    private final StructureType type;
    private final IntBoundingBox mainBox;
    private final ImmutableList<IntBoundingBox> componentBoxes;
    private long refreshTime;

    private StructureData(StructureType type, ImmutableList<IntBoundingBox> componentBoxes, long refreshTime)
    {
        this(type, componentBoxes);

        this.refreshTime = refreshTime;
    }

    private StructureData(StructureType type, ImmutableList<IntBoundingBox> componentBoxes)
    {
        this.type = type;
        this.mainBox = encompass(componentBoxes);
        this.componentBoxes = componentBoxes;
    }

    public StructureType getStructureType()
    {
        return this.type;
    }

    public IntBoundingBox getBoundingBox()
    {
        return this.mainBox;
    }

    public ImmutableList<IntBoundingBox> getComponents()
    {
        return this.componentBoxes;
    }

    public long getRefreshTime()
    {
        return this.refreshTime;
    }

    public static StructureData fromStructureStart(StructureType type, StructureStart structure)
    {
        ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
        List<StructurePiece> components = structure.getChildren();

        for (StructurePiece component : components)
        {
            builder.add(IntBoundingBox.fromVanillaBox(component.getBoundingBox()));
        }

        return new StructureData(type, builder.build());
    }

    @Nullable
    public static StructureData fromStructureStartTag(NbtCompound tag, long currentTime)
    {
        if (tag.contains("id", Constants.NBT.TAG_STRING) &&
            tag.contains("Children", Constants.NBT.TAG_LIST))
        {
            StructureType type = StructureType.fromStructureId(tag.getString("id"));
            ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
            NbtList pieces = tag.getList("Children", Constants.NBT.TAG_COMPOUND);
            final int count = pieces.size();

            for (int i = 0; i < count; ++i)
            {
                NbtCompound pieceTag = pieces.getCompound(i);
                builder.add(IntBoundingBox.fromArray(pieceTag.getIntArray("BB")));
            }

            return new StructureData(type, builder.build(), currentTime);
        }

        return null;
    }

    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((this.componentBoxes == null) ? 0 : this.componentBoxes.hashCode());
        result = prime * result + ((this.mainBox == null) ? 0 : this.mainBox.hashCode());
        result = prime * result + ((this.type == null) ? 0 : this.type.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
        {
            return true;
        }

        if (obj == null || this.getClass() != obj.getClass())
        {
            return false;
        }

        StructureData other = (StructureData) obj;

        if (this.componentBoxes == null)
        {
            if (other.componentBoxes != null)
            {
                return false;
            }
        }
        else if (! this.componentBoxes.equals(other.componentBoxes))
        {
            return false;
        }

        if (this.mainBox == null)
        {
            if (other.mainBox != null)
            {
                return false;
            }
        }
        else if (! this.mainBox.equals(other.mainBox))
        {
            return false;
        }

        return this.type == other.type;
    }

    public static IntBoundingBox encompass(Iterable<IntBoundingBox> boxes)
    {
        Iterator<IntBoundingBox> iterator = boxes.iterator();

        if (iterator.hasNext())
        {
            IntBoundingBox box = iterator.next();
            int minX = box.minX;
            int minY = box.minY;
            int minZ = box.minZ;
            int maxX = box.maxX;
            int maxY = box.maxY;
            int maxZ = box.maxZ;

            while (iterator.hasNext())
            {
                box = iterator.next();
                minX = Math.min(minX, box.minX);
                minY = Math.min(minY, box.minY);
                minZ = Math.min(minZ, box.minZ);
                maxX = Math.max(maxX, box.maxX);
                maxY = Math.max(maxY, box.maxY);
                maxZ = Math.max(maxZ, box.maxZ);
            }

            return new IntBoundingBox(minX, minY, minZ, maxX, maxY, maxZ);
        }

        return new IntBoundingBox(0, 0, 0, 0, 0, 0);
    }
}
