package fi.dy.masa.minihud.util;

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
    public static final int CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX = 0;
    public static final int CARPET_STRUCTURE_ID_END_CITY = 1;
    public static final int CARPET_STRUCTURE_ID_FORTRESS = 2;
    public static final int CARPET_STRUCTURE_ID_TEMPLE = 3;
    public static final int CARPET_STRUCTURE_ID_VILLAGE = 4;
    public static final int CARPET_STRUCTURE_ID_STRONGHOLD = 5;
    public static final int CARPET_STRUCTURE_ID_MINESHAFT = 6;
    public static final int CARPET_STRUCTURE_ID_MONUMENT = 7;
    public static final int CARPET_STRUCTURE_ID_MANSION = 8;

    //private static final CarpetBoxReader CARPET_BOX_READER = new CarpetBoxReader();

    private final StructureType type;
    private final IntBoundingBox mainBox;
    private final ImmutableList<IntBoundingBox> componentBoxes;
    private long refreshTime;

    private StructureData(StructureType type, IntBoundingBox mainBox, ImmutableList<IntBoundingBox> componentBoxes, long refreshTime)
    {
        this(type, mainBox, componentBoxes);

        this.refreshTime = refreshTime;
    }

    private StructureData(StructureType type, IntBoundingBox mainBox, ImmutableList<IntBoundingBox> componentBoxes)
    {
        this.type = type;
        this.mainBox = mainBox;
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

    public static StructureData fromStructureStart(StructureType type, StructureStart<?> structure)
    {
        ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
        List<StructurePiece> components = structure.getChildren();

        for (StructurePiece component : components)
        {
            builder.add(IntBoundingBox.fromVanillaBox(component.getBoundingBox()));
        }

        return new StructureData(type, IntBoundingBox.fromVanillaBox(structure.getBoundingBox()), builder.build());
    }

    @Nullable
    public static StructureData fromStructureStartTag(NbtCompound tag, long currentTime)
    {
        if (tag.contains("BB", Constants.NBT.TAG_INT_ARRAY) &&
            tag.contains("Children", Constants.NBT.TAG_LIST))
        {
            StructureType type = StructureType.byStructureId(tag.getString("id"));

            ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
            NbtList pieces = tag.getList("Children", Constants.NBT.TAG_COMPOUND);
            final int count = pieces.size();

            for (int i = 0; i < count; ++i)
            {
                NbtCompound pieceTag = pieces.getCompound(i);
                builder.add(IntBoundingBox.fromArray(pieceTag.getIntArray("BB")));
            }

            return new StructureData(type, IntBoundingBox.fromArray(tag.getIntArray("BB")), builder.build(), currentTime);
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

        if (this.type != other.type)
        {
            return false;
        }

        return true;
    }
}
