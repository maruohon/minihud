package fi.dy.masa.minihud.data;

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
import fi.dy.masa.malilib.util.data.Constants;
import fi.dy.masa.malilib.util.position.IntBoundingBox;
import fi.dy.masa.malilib.util.wrap.NbtWrap;
import fi.dy.masa.minihud.MiniHUD;

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

    /**
     * Reads structures from the vanilla 1.12 and below structure files,
     * and adds any structures of the provided StructureType <b>type</b> to the provided map.
     * @param map
     * @param rootCompound
     * @param type
     */
    public static void readAndAddStructuresToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootCompound, StructureType type)
    {
        if (NbtWrap.containsCompound(rootCompound, "data"))
        {
            rootCompound = NbtWrap.getCompound(rootCompound, "data");

            if (NbtWrap.containsCompound(rootCompound, "Features"))
            {
                rootCompound = NbtWrap.getCompound(rootCompound, "Features");

                for (String key : NbtWrap.getKeys(rootCompound))
                {
                    NBTBase nbtBase = NbtWrap.getTag(rootCompound, key);

                    if (NbtWrap.getTypeId(nbtBase) == Constants.NBT.TAG_COMPOUND)
                    {
                        NBTTagCompound tag = (NBTTagCompound) nbtBase;
                        String id = NbtWrap.getString(tag, "id");

                        if (type.getStructureName().equals(id))
                        {
                            StructureData data = fromTag(tag);

                            if (data != null)
                            {
                                map.put(type, data);
                            }
                        }
                    }
                }
            }
        }
    }

    @Nullable
    protected static StructureData fromTag(NBTTagCompound tag)
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

    /**
     * Reads Temple structures from the vanilla 1.12 and below structure files,
     * and adds them to the provided map. The structure type is read from the child component. 
     */
    public static void readAndAddTemplesToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootTag)
    {
        if (NbtWrap.containsCompound(rootTag, "data") == false)
        {
            return;
        }

        NBTTagCompound dataTag = NbtWrap.getCompound(rootTag, "data");

        if (NbtWrap.containsCompound(dataTag, "Features") == false)
        {
            return;
        }

        NBTTagCompound featureTag = NbtWrap.getCompound(dataTag, "Features");

        for (String key : NbtWrap.getKeys(featureTag))
        {
            NBTBase nbtBase = NbtWrap.getTag(featureTag, key);

            if (NbtWrap.getTypeId(nbtBase) != Constants.NBT.TAG_COMPOUND)
            {
                continue;
            }

            NBTTagCompound tag = (NBTTagCompound) nbtBase;

            if (NbtWrap.contains(tag, "ChunkX", Constants.NBT.TAG_ANY_NUMERIC) &&
                NbtWrap.contains(tag, "ChunkZ", Constants.NBT.TAG_ANY_NUMERIC) &&
                NbtWrap.containsIntArray(tag, "BB") &&
                NbtWrap.getString(tag, "id").equals("Temple"))
            {
                NBTTagList tagList = NbtWrap.getListOfCompounds(tag, "Children");

                if (NbtWrap.getListSize(tagList) != 1)
                {
                    continue;
                }

                NBTTagCompound componentTag = NbtWrap.getCompoundAt(tagList, 0);

                if (NbtWrap.containsString(componentTag, "id") &&
                    NbtWrap.containsIntArray(componentTag, "BB"))
                {
                    String id = NbtWrap.getString(componentTag, "id");
                    StructureType type = StructureType.templeTypeFromComponentId(id);

                    if (type != null)
                    {
                        IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(componentTag, "BB"));
                        map.put(type, new StructureData(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "BB")), ImmutableList.of(bb)));
                    }
                }
            }
        }
    }

    public static void readStructureDataCarpetAllBoxes(ArrayListMultimap<StructureType, StructureData> map, NBTTagList tagList)
    {
        List<NBTTagCompound> tags = new ArrayList<>();
        final int size = NbtWrap.getListSize(tagList);

        for (int listNum = 0; listNum < size; ++listNum)
        {
            NBTTagList innerList = (NBTTagList) tagList.get(listNum);
            final int innerSize = NbtWrap.getListSize(innerList);

            for (int i = 0; i < innerSize; ++i)
            {
                tags.add(NbtWrap.getCompoundAt(innerList, i));
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
            int id = NbtWrap.getInt(tag, "type");

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
                    bbMain = IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb"));
                    id = NbtWrap.getInt(tags.get(i + 1), "type");
                    type = getTypeFromCarpetId(id);
                }
            }
            // Don't add the component boxes of unknown/unsupported structure types to the builder
            else if (type != null)
            {
                builder.add(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb")));
                ++componentBoxes;
            }
        }

        if (componentBoxes > 0 && type != null && bbMain != null)
        {
            map.put(type, new StructureData(bbMain, builder.build()));
        }
    }

    public static void readStructureDataServux(ArrayListMultimap<StructureType, StructureData> map, NBTTagList tagList)
    {
        final int size = NbtWrap.getListSize(tagList);

        for (int i = 0; i < size; ++i)
        {
            NBTTagCompound tag = NbtWrap.getCompoundAt(tagList, i);
            String id = NbtWrap.getString(tag, "id");
            StructureType type = StructureType.ID_TO_TYPE.get(id);
            StructureData data = fromTag(tag);

            if (type != null && data != null)
            {
                map.put(type, data);
            }
        }

        //System.out.printf("readStructureDataServux(), size: %d\n", size);
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
        int id = NbtWrap.getInt(tag, "type");
        IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "bb"));

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

            MiniHUD.logInfo("Structure data updated from Carpet server (split data), structures: {}", map.size());
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
