package fi.dy.masa.minihud.util;

import java.util.ArrayList;
import java.util.List;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import net.minecraft.world.gen.structure.StructureComponent;
import net.minecraft.world.gen.structure.StructureStart;
import fi.dy.masa.malilib.util.Constants;
import fi.dy.masa.malilib.util.IntBoundingBox;
import fi.dy.masa.minihud.MiniHud;

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

    private static final CarpetBoxReader CARPET_BOX_READER = new CarpetBoxReader();

    private final IntBoundingBox mainBox;
    private final ImmutableList<IntBoundingBox> componentBoxes;

    private StructureData(IntBoundingBox mainBox, ImmutableList<IntBoundingBox> componentBoxes)
    {
        this.mainBox = mainBox;
        this.componentBoxes = componentBoxes;
    }

    public IntBoundingBox getBoundingBox()
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
                                ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();

                                for (int i = 0; i < tagList.tagCount(); ++i)
                                {
                                    NBTTagCompound componentTag = tagList.getCompoundTagAt(i);

                                    if (componentTag.hasKey("id", Constants.NBT.TAG_STRING) &&
                                        componentTag.hasKey("BB", Constants.NBT.TAG_INT_ARRAY))
                                    {
                                        builder.add(IntBoundingBox.fromArray(componentTag.getIntArray("BB")));
                                    }
                                }

                                map.put(type, new StructureData(IntBoundingBox.fromArray(tag.getIntArray("BB")), builder.build()));
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
                                        IntBoundingBox bb = IntBoundingBox.fromArray(componentTag.getIntArray("BB"));
                                        map.put(type, new StructureData(IntBoundingBox.fromArray(tag.getIntArray("BB")), ImmutableList.of(bb)));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static void readStructureDataCarpetAllBoxes(ArrayListMultimap<StructureType, StructureData> map, NBTTagList tagList)
    {
        List<NBTTagCompound> tags = new ArrayList<>();

        for (int listNum = 0; listNum < tagList.tagCount(); ++listNum)
        {
            NBTTagList innerList = (NBTTagList) tagList.get(listNum);

            for (int i = 0; i < innerList.tagCount(); ++i)
            {
                tags.add(innerList.getCompoundTagAt(i));
            }
        }

        //System.out.printf("SD - readStructureDataCarpetAllBoxes, list: %d\n", tags.size());
        readStructureDataCarpetAllBoxes(map, tags);
    }

    public static void readStructureDataCarpetAllBoxes(ArrayListMultimap<StructureType, StructureData> map, List<NBTTagCompound> tags)
    {
        ImmutableList.Builder<IntBoundingBox> builder = ImmutableList.builder();
        IntBoundingBox bbMain = null;
        StructureType type = null;
        int componentBoxes = 0;

        for (int i = 0; i < tags.size(); ++i)
        {
            NBTTagCompound tag = tags.get(i);
            int id = tag.getInteger("type");

            // Carpet puts the main box as the first entry in the same list of compound tags
            // Only the subsequent component boxes will have the structure type ID
            if (id == CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX)
            {
                // The beginning of another structure
                if (type != null && bbMain != null)
                {
                    map.put(type, new StructureData(bbMain, builder.build()));
                    builder = ImmutableList.builder();
                    type = null;
                }

                if (tags.size() > i + 1)
                {
                    bbMain = IntBoundingBox.fromArray(tag.getIntArray("bb"));
                    id = tags.get(i + 1).getInteger("type");
                    type = getTypeFromCarpetId(id);
                }
            }
            // Don't add the component boxes of unknown/unsupported structure types to the builder
            else if (type != null)
            {
                builder.add(IntBoundingBox.fromArray(tag.getIntArray("bb")));
                ++componentBoxes;
            }
        }

        if (componentBoxes > 0 && type != null && bbMain != null)
        {
            map.put(type, new StructureData(bbMain, builder.build()));
        }
    }

    private static void resetCarpetBoxReader()
    {
        CARPET_BOX_READER.expectedBoxes = -1;
        CARPET_BOX_READER.seenBoxes = 0;
        CARPET_BOX_READER.componentBoxes = 0;
        CARPET_BOX_READER.componentsBuilder = null;
        CARPET_BOX_READER.bbMain = null;
        CARPET_BOX_READER.readTypeFromNextBox = false;
        CARPET_BOX_READER.type = null;
    }

    public static void readStructureDataCarpetIndividualBoxesHeader(int boxCount)
    {
        CARPET_BOX_READER.expectedBoxes = boxCount;
        resetCarpetBoxReader();
    }

    public static void readStructureDataCarpetIndividualBoxes(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound tag)
    {
        int id = tag.getInteger("type");
        IntBoundingBox bb = IntBoundingBox.fromArray(tag.getIntArray("bb"));

        CARPET_BOX_READER.seenBoxes++;

        if (id == CARPET_STRUCTURE_ID_OUTER_BOUNDING_BOX)
        {
            if (CARPET_BOX_READER.type != null &&
                CARPET_BOX_READER.bbMain != null &&
                CARPET_BOX_READER.componentBoxes > 0)
            {
                map.put(CARPET_BOX_READER.type, new StructureData(CARPET_BOX_READER.bbMain, CARPET_BOX_READER.componentsBuilder.build()));
            }

            CARPET_BOX_READER.bbMain = bb;
            CARPET_BOX_READER.readTypeFromNextBox = true;
            CARPET_BOX_READER.type = null;
            CARPET_BOX_READER.componentsBuilder = ImmutableList.builder();
        }
        else
        {
            CARPET_BOX_READER.componentBoxes++;

            if (CARPET_BOX_READER.readTypeFromNextBox)
            {
                CARPET_BOX_READER.type = getTypeFromCarpetId(id);
                CARPET_BOX_READER.readTypeFromNextBox = false;
            }

            if (CARPET_BOX_READER.componentsBuilder != null)
            {
                CARPET_BOX_READER.componentsBuilder.add(bb);
            }
        }

        if (CARPET_BOX_READER.seenBoxes >= CARPET_BOX_READER.expectedBoxes)
        {
            if (CARPET_BOX_READER.type != null &&
                CARPET_BOX_READER.bbMain != null &&
                CARPET_BOX_READER.componentBoxes > 0)
            {
                map.put(CARPET_BOX_READER.type, new StructureData(CARPET_BOX_READER.bbMain, CARPET_BOX_READER.componentsBuilder.build()));
            }

            resetCarpetBoxReader();

            MiniHud.logger.info("Structure data updated from Carpet server (split data), structures: {}", map.size());
        }
    }

    @Nullable
    private static StructureType getTypeFromCarpetId(int id)
    {
        switch (id)
        {
            case CARPET_STRUCTURE_ID_END_CITY:      return StructureType.END_CITY;
            case CARPET_STRUCTURE_ID_FORTRESS:      return StructureType.NETHER_FORTRESS;
            case CARPET_STRUCTURE_ID_MANSION:       return StructureType.MANSION;
            case CARPET_STRUCTURE_ID_MONUMENT:      return StructureType.OCEAN_MONUMENT;
            case CARPET_STRUCTURE_ID_STRONGHOLD:    return StructureType.STRONGHOLD;
            case CARPET_STRUCTURE_ID_TEMPLE:        return StructureType.WITCH_HUT;
            case CARPET_STRUCTURE_ID_VILLAGE:       return StructureType.VILLAGE;
        }

        return null;
    }

    private static class CarpetBoxReader
    {
        private int expectedBoxes = -1;
        private int seenBoxes;
        private int componentBoxes;
        private ImmutableList.Builder<IntBoundingBox> componentsBuilder;
        private IntBoundingBox bbMain;
        private boolean readTypeFromNextBox;
        private StructureType type;
    }
}
