package fi.dy.masa.minihud.data.structure;

import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ImmutableList;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.gen.structure.StructureComponent;
import net.minecraft.world.gen.structure.StructureStart;

import malilib.util.data.Constants;
import malilib.util.game.wrap.NbtWrap;
import malilib.util.position.IntBoundingBox;

public class StructureData
{
    private final IntBoundingBox mainBox;
    private final ImmutableList<IntBoundingBox> componentBoxes;

    public StructureData(IntBoundingBox mainBox, ImmutableList<IntBoundingBox> componentBoxes)
    {
        this.mainBox = mainBox;
        this.componentBoxes = componentBoxes;
    }

    public IntBoundingBox getEnclosingBox()
    {
        return this.mainBox;
    }

    public ImmutableList<IntBoundingBox> getComponents()
    {
        return this.componentBoxes;
    }

    public static StructureData fromStructure(StructureStart structure)
    {
        ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
        List<StructureComponent> components = structure.getComponents();

        for (StructureComponent component : components)
        {
            builder.add(IntBoundingBox.fromVanillaBox(component.getBoundingBox()));
        }

        return new StructureData(IntBoundingBox.fromVanillaBox(structure.getBoundingBox()), builder.build());
    }

    @Nullable
    public static StructureData fromTag(NBTTagCompound tag)
    {
        if (NbtWrap.contains(tag, "ChunkX", Constants.NBT.TAG_ANY_NUMERIC) &&
            NbtWrap.contains(tag, "ChunkZ", Constants.NBT.TAG_ANY_NUMERIC) &&
            NbtWrap.containsIntArray(tag, "BB"))
        {
            NBTTagList tagList = NbtWrap.getListOfCompounds(tag, "Children");
            ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
            final int size = NbtWrap.getListSize(tagList);

            for (int i = 0; i < size; ++i)
            {
                NBTTagCompound componentTag = NbtWrap.getCompoundAt(tagList, i);

                if (NbtWrap.containsString(componentTag, "id") &&
                    NbtWrap.containsIntArray(componentTag, "BB"))
                {
                    builder.add(IntBoundingBox.fromArray(NbtWrap.getIntArray(componentTag, "BB")));
                }
            }

            return new StructureData(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "BB")), builder.build());
        }

        return null;
    }
}
