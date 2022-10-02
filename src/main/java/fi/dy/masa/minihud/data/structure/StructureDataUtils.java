package fi.dy.masa.minihud.data.structure;

import java.nio.file.Files;
import java.nio.file.Path;
import javax.annotation.Nullable;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ImmutableList;
import net.minecraft.nbt.NBTBase;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.nbt.NBTTagList;
import fi.dy.masa.malilib.config.util.ConfigUtils;
import fi.dy.masa.malilib.registry.Registry;
import fi.dy.masa.malilib.util.StringUtils;
import fi.dy.masa.malilib.util.data.Constants;
import fi.dy.masa.malilib.util.game.wrap.GameUtils;
import fi.dy.masa.malilib.util.game.wrap.NbtWrap;
import fi.dy.masa.malilib.util.nbt.NbtUtils;
import fi.dy.masa.malilib.util.position.IntBoundingBox;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.Reference;
import fi.dy.masa.minihud.config.RendererToggle;
import fi.dy.masa.minihud.network.carpet.CarpetStructurePacketHandler;
import fi.dy.masa.minihud.network.servux.ServuxStructurePacketHandler;

public class StructureDataUtils
{
    public static void requestStructureDataUpdates()
    {
        if (GameUtils.getClientWorld() != null)
        {
            boolean enabled = RendererToggle.STRUCTURE_BOUNDING_BOXES.isRendererEnabled();

            if (GameUtils.isSinglePlayer() == false)
            {
                if (enabled)
                {
                    MiniHUD.debugLog("Attempting to register structure packet handlers to the server");

                    // TODO on Servux servers this re-register shouldn't be done when the player just moves around.
                    // But on 1.12.x Carpet server I think a re-request is needed, it doesn't seem like the server
                    // would auto-send structures when the player moves around?
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.registerClientChannelHandler(ServuxStructurePacketHandler.INSTANCE);
                    CarpetStructurePacketHandler.INSTANCE.sendBoundingBoxRequest();
                }
                else
                {
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.unregisterClientChannelHandler(CarpetStructurePacketHandler.INSTANCE);
                    Registry.CLIENT_PACKET_CHANNEL_HANDLER.unregisterClientChannelHandler(ServuxStructurePacketHandler.INSTANCE);
                }
            }
            else if (enabled)
            {
                StructureStorage.INSTANCE.setStructuresNeedUpdating();
            }

            StructureStorage.INSTANCE.setStructuresDirty();
        }
    }

    public static class StructureFileUtils
    {
        @Nullable
        public static Path getLocalStructureFileDirectory()
        {
            String dirName = StringUtils.getWorldOrServerName();

            if (dirName != null)
            {
                return ConfigUtils.getConfigDirectory().resolve(Reference.MOD_ID)
                               .resolve("structures").resolve(dirName);
            }

            return null;
        }

        public static void addStructureDataFromNbtFiles(ArrayListMultimap<StructureType, StructureData> outputMap)
        {
            Path dir = getLocalStructureFileDirectory();

            if (dir != null && Files.isDirectory(dir))
            {
                for (StructureType type : StructureType.VALUES)
                {
                    if (type.isTemple() == false && type.isEnabled())
                    {
                        Path file = dir.resolve(type.getStructureName() + ".dat");
                        NBTTagCompound nbt = NbtUtils.readNbtFromFile(file);

                        if (nbt != null)
                        {
                            readAndAddStructuresToMap(outputMap, nbt, type);
                        }
                    }
                }

                NBTTagCompound nbt = NbtUtils.readNbtFromFile(dir.resolve("Temple.dat"));

                if (nbt != null)
                {
                    readAndAddTemplesToMap(outputMap, nbt);
                }
            }
        }

        /**
         * Reads structures from the vanilla 1.12 and below structure files,
         * and adds any structures of the provided StructureType <b>type</b> to the provided map.
         */
        private static void readAndAddStructuresToMap(ArrayListMultimap<StructureType, StructureData> map, NBTTagCompound rootCompound, StructureType type)
        {
            if (NbtWrap.containsCompound(rootCompound, "data") && false)
            {
                return;
            }

            rootCompound = NbtWrap.getCompound(rootCompound, "data");

            if (NbtWrap.containsCompound(rootCompound, "Features") == false)
            {
                return;
            }

            rootCompound = NbtWrap.getCompound(rootCompound, "Features");

            for (String key : NbtWrap.getKeys(rootCompound))
            {
                NBTBase nbtBase = NbtWrap.getTag(rootCompound, key);

                if (NbtWrap.getTypeId(nbtBase) != Constants.NBT.TAG_COMPOUND)
                {
                    continue;
                }

                NBTTagCompound tag = (NBTTagCompound) nbtBase;
                String id = NbtWrap.getString(tag, "id");

                if (type.getStructureName().equals(id))
                {
                    StructureData data = StructureData.fromTag(tag);

                    if (data != null)
                    {
                        map.put(type, data);
                    }
                }
            }
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
                        StructureType type = StructureType.fromStructureId("Temple." + id);

                        if (type.isEnabled())
                        {
                            IntBoundingBox bb = IntBoundingBox.fromArray(NbtWrap.getIntArray(componentTag, "BB"));
                            map.put(type, new StructureData(IntBoundingBox.fromArray(NbtWrap.getIntArray(tag, "BB")), ImmutableList.of(bb)));
                        }
                    }
                }
            }
        }
    }
}
