package fi.dy.masa.minihud.util;

import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import fi.dy.masa.malilib.util.Constants;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.gen.structure.StructureBoundingBox;
import net.minecraft.world.gen.structure.StructureComponent;
import net.minecraft.world.gen.structure.StructureStart;

public class StructureData
{
    private final StructureBoundingBox mainBox;
    private final ImmutableList<StructureBoundingBox> componentBoxes;

    private StructureData(StructureBoundingBox mainBox, ImmutableList<StructureBoundingBox> componentBoxes)
    {
        this.mainBox = mainBox;
        this.componentBoxes = componentBoxes;
    }

    public StructureBoundingBox getBoundingBox()
    {
        return this.mainBox;
    }

    public ImmutableList<StructureBoundingBox> getComponents()
    {
        return this.componentBoxes;
    }

    public static StructureData fromStructure(StructureStart structure)
    {
        ImmutableList.Builder<StructureBoundingBox> builder = ImmutableList.builder();
        List<StructureComponent> components = structure.getComponents();

        for (StructureComponent component : components)
        {
            builder.add(component.getBoundingBox());
        }

        return new StructureData(structure.getBoundingBox(), builder.build());
    }

    @Nullable
    public static void readAndAddStructuresToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootCompound, StructureType type)
    {
        if (rootCompound.hasKey("data", Constants.NBT.TAG_COMPOUND))
        {
            rootCompound = rootCompound.getCompoundTag("data");

            if (rootCompound.hasKey("Features", Constants.NBT.TAG_COMPOUND))
            {
                rootCompound = rootCompound.getCompoundTag("Features");

                for (String key : rootCompound.getKeySet())
                {
                    NBTBase nbtBase = rootCompound.getTag(key);

                    if (nbtBase.getId() == Constants.NBT.TAG_COMPOUND)
                    {
                        NBTTagCompound tag = (NBTTagCompound) nbtBase;

                        if (tag.hasKey("ChunkX") && tag.hasKey("ChunkZ") && tag.hasKey("BB", Constants.NBT.TAG_INT_ARRAY))
                        {
                            String id = tag.getString("id");

                            if (type.getStructureName().equals(id))
                            {
                                NBTTagList tagList = tag.getTagList("Children", Constants.NBT.TAG_COMPOUND);
                                ImmutableList.Builder<StructureBoundingBox> builder = ImmutableList.builder();

                                for (int i = 0; i < tagList.tagCount(); ++i)
                                {
                                    NBTTagCompound componentTag = tagList.getCompoundTagAt(i);

                                    if (componentTag.hasKey("id", Constants.NBT.TAG_STRING) &&
                                        componentTag.hasKey("BB", Constants.NBT.TAG_INT_ARRAY))
                                    {
                                        builder.add(new StructureBoundingBox(componentTag.getIntArray("BB")));
                                    }
                                }

                                map.put(type, new StructureData(new StructureBoundingBox(tag.getIntArray("BB")), builder.build()));
                            }
                        }
                    }
                }
            }
        }
    }

    @Nullable
    public static void readAndAddTemplesToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootCompound)
    {
        if (rootCompound.hasKey("data", Constants.NBT.TAG_COMPOUND))
        {
            rootCompound = rootCompound.getCompoundTag("data");

            if (rootCompound.hasKey("Features", Constants.NBT.TAG_COMPOUND))
            {
                rootCompound = rootCompound.getCompoundTag("Features");

                for (String key : rootCompound.getKeySet())
                {
                    NBTBase nbtBase = rootCompound.getTag(key);

                    if (nbtBase.getId() == Constants.NBT.TAG_COMPOUND)
                    {
                        NBTTagCompound tag = (NBTTagCompound) nbtBase;

                        if (tag.hasKey("ChunkX") && tag.hasKey("ChunkZ") &&
                            tag.hasKey("BB", Constants.NBT.TAG_INT_ARRAY) &&
                            tag.getString("id").equals("Temple"))
                        {
                            NBTTagList tagList = tag.getTagList("Children", Constants.NBT.TAG_COMPOUND);

                            if (tagList.tagCount() == 1)
                            {
                                NBTTagCompound componentTag = tagList.getCompoundTagAt(0);

                                if (componentTag.hasKey("id", Constants.NBT.TAG_STRING) &&
                                    componentTag.hasKey("BB", Constants.NBT.TAG_INT_ARRAY))
                                {
                                    String id = componentTag.getString("id");
                                    StructureType type = StructureType.templeTypeFromComponentId(id);

                                    if (type != null)
                                    {
                                        StructureBoundingBox bb = new StructureBoundingBox(componentTag.getIntArray("BB"));
                                        map.put(type, new StructureData(new StructureBoundingBox(tag.getIntArray("BB")), ImmutableList.of(bb)));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
