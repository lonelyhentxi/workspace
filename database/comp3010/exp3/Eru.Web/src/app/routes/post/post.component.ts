import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { HttpClient } from '@angular/common/http';
import { ApiService } from '@core/eru/api.service';
import { PostDto } from '@core/eru/dtos/post.dto';
import { CommentDto, Comment } from '@core/eru/dtos/comment.dto';
import { SettingsService } from '@delon/theme';

@Component({
  selector: 'app-post',
  templateUrl: './post.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: [],
})
export class PostComponent implements OnInit {

  private post: PostDto = null;
  private commentsTree: Comment[] = [];
  inputValue = '';
  submitting = false;
  parentId?: string = null;
  parentText = '';

  constructor(
    private route: ActivatedRoute,
    private httpClient: HttpClient,
    private apiService: ApiService,
    private cdr: ChangeDetectorRef,
    private appSettingService: SettingsService,
  ) {
  }

  async ngOnInit() {
    this.route.params.subscribe(async params => {
      await this.loadData(params.id);
    });
  }

  async loadData(id: string) {
    this.httpClient.get(this.apiService.apiJoin(`posts/${id}`))
      .subscribe(postRes => {
        const post = (postRes as any).body;
        this.post = post;
        this.commentsTree.splice(0, this.commentsTree.length, ...this.convertComments(post.Comments));
        setTimeout(() => {
          this.cdr.detectChanges();
        }, 300);
      });
  }

  get user() {
    return this.appSettingService.user;
  }

  setParent(parent: Comment) {
    if(parent) {
      this.parentId = parent.id;
      this.parentText = parent.content;
    } else {
      this.parentId = null;
      this.parentText = '';
    }
    setTimeout(() => {
      this.cdr.detectChanges();
    }, 50);
  }

  convertComments(comments: CommentDto[]) {
    const commentMap = new Map<string, Comment>();
    const commentSet = new Set<string>();
    for (const comment of comments) {
      commentSet.add(comment.Id);
      commentMap.set(comment.Id, {
        id: comment.Id,
        author: comment.User.Name,
        avatar: comment.User.Avatar,
        content: comment.Content,
        createTime: new Date(comment.CreateTime).toLocaleDateString(),
      });
    }
    for (const comment of comments) {
      if (!comment.ParentId || !commentSet.has(comment.ParentId) || comment.ParentId === comment.Id) {
        comment.ParentId = null;
      }
    }
    for (const comment of comments) {
      const current = commentMap.get(comment.Id);
      if (comment.ParentId) {
        const parentId = comment.ParentId;
        const parent = commentMap.get(parentId);
        if (!parent.children) {
          parent.children = [current];
        } else {
          parent.children.push(current);
        }
      }
    }
    const res = [];
    for (const comment of comments) {
      if (!comment.ParentId) {
        res.push(commentMap.get(comment.Id));
      }
    }
    return res;
  }

  handleSubmit(): void {
    this.submitting = true;
    const content = this.inputValue;
    console.log(this.apiService.apiCache.user);
    this.httpClient.post(this.apiService.apiJoin('comments'), {
      UserId: this.appSettingService.user.id,
      PostId: this.post.Id,
      CategoryId: 1,
      StatusId: 1,
      Content: content,
      ParentId: this.parentId,
    }).subscribe(() => {
      this.loadData(this.post.Id)
        .then(() => {
        })
        .finally(() => {
          this.inputValue = '';
          this.setParent(null);
          this.submitting = false;
        });
    },(err)=>{this.submitting=false;});
  }
}
