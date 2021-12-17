package fi.dy.masa.minihud.mixin;

import java.util.Set;
import java.util.UUID;
import org.spongepowered.asm.mixin.Final;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Shadow;
import net.minecraft.server.world.ServerEntityManager;
import net.minecraft.world.entity.EntityIndex;
import fi.dy.masa.minihud.util.IServerEntityManager;

@Mixin(ServerEntityManager.class)
public abstract class MixinServerEntityManager implements IServerEntityManager
{
    @Shadow @Final Set<UUID> entityUuids;
    @Shadow @Final private EntityIndex<?> index;

    @Override
    public int getUuidSize()
    {
        return this.entityUuids.size();
    }

    @Override
    public int getIndexSize()
    {
        return this.index.size();
    }
}
