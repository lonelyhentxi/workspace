using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using Autofac;
using MediatR;
using Xunit;

namespace LearnLibs.Tests
{
    public class MediatRTest
    {
        public class Ping : IRequest<string>
        {
        }

        public class PingHandler : IRequestHandler<Ping, string>
        {
            public async Task<string> Handle(Ping request, CancellationToken cancellationToken)
            {
                return await Task.FromResult("pong");
            }
        };

        [Fact]
        public async Task ShowSingleCast()
        {
            var mediator = MediatRUtils.BuildMediator();
            var response = await mediator.Send(new Ping { });
            Assert.Equal("pong", response);
        }

        public class Notice : INotification
        {
            public static int Counter = 0;
        }

        public class Action1 : INotificationHandler<Notice>
        {
            public Task Handle(Notice notification, CancellationToken cancellationToken)
            {
                Notice.Counter += 1;
                return Task.CompletedTask;
            }
        }

        public class Action2 : INotificationHandler<Notice>
        {
            public Task Handle(Notice notification, CancellationToken cancellationToken)
            {
                Notice.Counter += 1;
                return Task.CompletedTask;
            }
        }

        [Fact]
        public async Task ShowMultiCast()
        {
            var meidator = MediatRUtils.BuildMediator();
            await meidator.Publish(new Notice { });
            Assert.Equal(2, Notice.Counter);
        }
    }
}