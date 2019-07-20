export interface Comment {
  id: string;
  author: string;
  content: string;
  avatar: string;
  children?: Comment[];
  createTime: string;
}

export class CommentDto {
  public Id: string;
  public User: { Name: string, Avatar: string };
  public ParentId?: string;
  public Content: string;
  public CreateTime: string;
}
