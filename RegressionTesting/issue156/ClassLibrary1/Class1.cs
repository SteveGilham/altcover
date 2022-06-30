using System;
using System.Linq;
using System.Windows.Threading;

namespace projectNamespace
{
    /// <summary>
    /// Invoke the callback in the SynchronizationContext with given priority.
    /// </summary>
    public interface IUiSynchronizationContext
    {
        /// <summary>
        /// Asynchronously invoke the callback in the SynchronizationContext with given priority.
        /// </summary>
        void Post(Action action, DispatcherPriority priority = DispatcherPriority.Normal);

        /// <summary>
        /// Synchronously invoke the callback in the SynchronizationContext with given priority.
        /// </summary>
        void Send(Action action, DispatcherPriority priority = DispatcherPriority.Normal);
    }
}
