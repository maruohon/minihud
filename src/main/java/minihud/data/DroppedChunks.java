package minihud.data;

import net.minecraft.world.WorldServer;

import malilib.util.game.wrap.GameUtils;
import minihud.config.Configs;
import minihud.util.MiscUtils;

public class DroppedChunks
{
    public static HashSizeType getDroppedChunksHashSizeType()
    {
        int size = Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

        if (size != -1)
        {
            return HashSizeType.CONFIG;
        }

        if (DataStorage.INSTANCE.worldProperties.droppedChunksHashSize.isPresent())
        {
            return HashSizeType.CARPET;
        }

        if (GameUtils.isSinglePlayer() && GameUtils.getClientWorld() != null)
        {
            return HashSizeType.SINGLE_PLAYER;
        }

        return HashSizeType.FALLBACK;
    }

    public static int getDroppedChunksHashSize()
    {
        HashSizeType type = getDroppedChunksHashSizeType();

        switch (type)
        {
            case CONFIG:
                return Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue();

            case SINGLE_PLAYER:
                WorldServer world = GameUtils.getClientPlayersServerWorld();
                return world != null ? MiscUtils.getCurrentHashSize(world) : 0xFFFF;

            case CARPET:
                WorldProperties prop = DataStorage.INSTANCE.worldProperties;
                if (prop.droppedChunksHashSize.isPresent())
                {
                    return prop.droppedChunksHashSize.getAsInt();
                }

            case FALLBACK:
            default:
                return 0xFFFF;
        }
    }

    public enum HashSizeType
    {
        CARPET          ("Carpet"),
        CONFIG          ("Config"),
        SINGLE_PLAYER   ("SP"),
        FALLBACK        ("0xFFFF");

        private final String displayName;

        HashSizeType(String displayName)
        {
            this.displayName = displayName;
        }

        public String getDisplayName()
        {
            return this.displayName;
        }
    }
}
